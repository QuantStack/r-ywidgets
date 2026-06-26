const fs = require("fs");
const path = require("path");
const { test, expect, galata } = require("@jupyterlab/galata");

const fileName = "widgets.ipynb";
const notebookPath = path.resolve(__dirname, `../../examples/${fileName}`);

test.use({ tmpPath: "r-ywidgets-test" });

/** Parse the example notebook with all cell outputs cleared. */
function loadClearedNotebook() {
  const nb = JSON.parse(fs.readFileSync(notebookPath, "utf8"));
  for (const cell of nb.cells) {
    if (cell.cell_type === "code") {
      cell.outputs = [];
      cell.execution_count = null;
    }
  }
  return nb;
}

/**
 * Galata has no "find cell by content" helper, and getCellTextInput reads the
 * editor via the clipboard (flaky/empty on some browsers). Since we own the
 * notebook, read its JSON directly: the cell order matches the rendered order,
 * so the index is deterministic and needs no browser interaction.
 */
function cellIndexBySource(nb, snippet) {
  const index = nb.cells.findIndex((cell) =>
    [].concat(cell.source).join("").includes(snippet),
  );
  if (index < 0) {
    throw new Error(`No cell found containing: ${snippet}`);
  }
  return index;
}

/** Run a cell and assert it executed without error output. */
async function runCellOk(page, cellIndex) {
  expect(await page.notebook.runCell(cellIndex)).toBe(true);

  const cell = await page.notebook.getCellLocator(cellIndex);
  // It got an execution count, i.e. the kernel actually ran it.
  await expect(cell.locator(".jp-InputPrompt")).toHaveText(/\[\d+\]/);
  // No error/traceback output.
  await expect(
    cell.locator('[data-mime-type="application/vnd.jupyter.error"]'),
  ).toHaveCount(0);
  return cell;
}

test.describe("examples/widgets.ipynb", () => {
  test.beforeEach(async ({ request, tmpPath }) => {
    const contents = galata.newContentsHelper(request);
    // Upload a copy with outputs cleared so assertions reflect this run only.
    await contents.uploadContent(
      JSON.stringify(loadClearedNotebook()),
      "text",
      `${tmpPath}/${fileName}`,
    );
  });

  test.afterEach(async ({ request, tmpPath }) => {
    const contents = galata.newContentsHelper(request);
    await contents.deleteDirectory(tmpPath);
  });

  test("defines and displays an IntSlider", async ({ page, tmpPath }) => {
    await page.notebook.openByPath(`${tmpPath}/${fileName}`);
    await page.notebook.activate(fileName);

    const nb = loadClearedNotebook();

    // Define the widget class.
    await runCellOk(
      page,
      cellIndexBySource(nb, "IntSlider <- ywidgets::make_comm_widget"),
    );

    // Instantiate and display it; the yjs-widgets frontend should render output.
    const displayCell = await runCellOk(
      page,
      cellIndexBySource(nb, "s <- IntSlider$new()"),
    );
    await expect(displayCell.locator(".jp-OutputArea-output")).toBeVisible();
  });
});
