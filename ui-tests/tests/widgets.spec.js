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
  const index = nb.cells.findIndex((cell) => [].concat(cell.source).join("").includes(snippet));
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
  await expect(cell.locator('[data-mime-type="application/vnd.jupyter.error"]')).toHaveCount(0);
  return cell;
}

/** Locator for a cell's rendered output area. */
function cellOutput(cell) {
  return cell.locator(".jp-OutputArea-output");
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
    await runCellOk(page, cellIndexBySource(nb, "IntSlider <- ywidgets::make_comm_widget"));

    // Instantiate and display it; the yjs-widgets frontend should render output.
    const displayCell = await runCellOk(page, cellIndexBySource(nb, "s <- IntSlider$new()"));
    await expect(cellOutput(displayCell)).toBeVisible();
  });

  test("renders the IntSlider and syncs UI changes back to R", async ({ page, tmpPath }) => {
    await page.notebook.openByPath(`${tmpPath}/${fileName}`);
    await page.notebook.activate(fileName);

    const nb = loadClearedNotebook();

    await runCellOk(page, cellIndexBySource(nb, "IntSlider <- ywidgets::make_comm_widget"));
    const displayCell = await runCellOk(page, cellIndexBySource(nb, "s <- IntSlider$new()"));

    // The yjs-widgets IntSlider renders a native HTML range input.
    const slider = displayCell.locator('input[type="range"]');
    await expect(slider).toBeVisible();
    await expect(slider).toHaveAttribute("min", "0");
    await expect(slider).toHaveAttribute("max", "100");
    await expect(slider).toHaveAttribute("step", "1");
    await expect(slider).toHaveValue("50"); // default value = 50L

    // Move the slider via the UI; this should sync to R over the comm channel.
    await slider.fill("75");
    await expect(slider).toHaveValue("75");

    // Re-evaluate `s$value` until R observes the new value (sync is async).
    const valueCellIndex = cellIndexBySource(nb, "s$value");
    await expect(async () => {
      expect(await page.notebook.runCell(valueCellIndex)).toBe(true);
      const valueCell = await page.notebook.getCellLocator(valueCellIndex);
      await expect(cellOutput(valueCell)).toContainText("75");
    }).toPass();

    // Reverse direction: set the value from R; the slider UI should follow.
    await runCellOk(page, cellIndexBySource(nb, "s$value = 0"));
    await expect(slider).toHaveValue("0");

    // And re-evaluating `s$value` reflects the same value.
    await expect(async () => {
      expect(await page.notebook.runCell(valueCellIndex)).toBe(true);
      const valueCell = await page.notebook.getCellLocator(valueCellIndex);
      await expect(cellOutput(valueCell)).toContainText("0");
    }).toPass();
  });
});
