// @ts-check
import { test, expect } from '@playwright/test';

/**
 * Phase 0 Basic Editing Tests
 * Tests from ManualTestingPlan.md - P0-01 through P0-06
 */

test.describe('Phase 0: Basic Text Input', () => {

  test.beforeEach(async ({ page }) => {
    // Go to the app
    await page.goto('/');

    // Wait for editor to be ready
    await page.waitForSelector('.editor-wrapper', { timeout: 10000 });

    // Focus the editor by clicking it
    await page.click('.editor-wrapper');
  });

  test('P0-01: Basic text input', async ({ page }) => {
    // Type a sentence
    const sentence = 'The quick brown fox jumps over the lazy dog.';
    await page.keyboard.type(sentence, { delay: 10 });

    // Wait for updates to propagate
    await page.waitForTimeout(100);

    // Verify text appears in the buffer
    const editorText = await page.textContent('.editable-area');
    expect(editorText).toContain(sentence);
  });

  test('P0-02: Enter/Return key creates newline', async ({ page }) => {
    // Type first line
    await page.keyboard.type('line 1', { delay: 10 });

    // Press Enter
    await page.keyboard.press('Enter');
    await page.waitForTimeout(50);

    // Type second line
    await page.keyboard.type('line 2', { delay: 10 });
    await page.waitForTimeout(100);

    // Verify both lines are present
    const editorText = await page.textContent('.editable-area');
    expect(editorText).toContain('line 1');
    expect(editorText).toContain('line 2');
  });

  test('P0-03: Backspace deletes character', async ({ page }) => {
    // Type text
    await page.keyboard.type('abcde', { delay: 10 });
    await page.waitForTimeout(50);

    // Press Backspace twice
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(50);
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(100);

    // Verify text is now "abc"
    const editorText = await page.textContent('.editable-area');
    expect(editorText).toContain('abc');
    expect(editorText).not.toContain('de');
  });

  /**
   * CRITICAL BUG TEST: P0-01 → P0-02 Transition
   *
   * This is the bug you found during manual testing:
   * 1. Type some text (P0-01)
   * 2. Backspace everything to get a clean slate
   * 3. Try to type again (P0-02)
   * 4. BUG: Nothing appears on screen - editor is stuck
   *
   * Root cause: When backspace at position 0, the transaction queue
   * gets stuck with in-flight flag never cleared.
   */
  test('REGRESSION: Typing works after backspacing entire buffer (P0-01 → P0-02)', async ({ page }) => {
    console.log('Step 1: Type initial text (P0-01)');
    await page.keyboard.type('abcd', { delay: 10 });
    await page.waitForTimeout(100);

    let editorText = await page.textContent('.editable-area');
    expect(editorText).toContain('abcd');
    console.log('✓ Initial text present:', editorText);

    console.log('Step 2: Backspace everything to clear buffer');
    // Backspace all 4 characters
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(50);
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(50);
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(50);
    await page.keyboard.press('Backspace');
    await page.waitForTimeout(100);

    editorText = await page.textContent('.editable-area');
    console.log('✓ Buffer cleared:', editorText);

    console.log('Step 3: Try to type again (P0-02)');
    // THIS IS WHERE THE BUG MANIFESTS - typing doesn't work
    await page.keyboard.type('line 1', { delay: 10 });

    // Wait a bit for any async processing
    await page.waitForTimeout(100);

    editorText = await page.textContent('.editable-area');
    console.log('Final text:', editorText);

    // This assertion will FAIL if the bug exists
    expect(editorText).toContain('line 1');
  });

  test('P0-04: Delete key deletes character forward', async ({ page }) => {
    // Type text
    await page.keyboard.type('abcde', { delay: 10 });
    await page.waitForTimeout(50);

    // Move cursor to between 'b' and 'c' using left arrow
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(30);
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(30);
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(50);

    // Press Delete twice
    await page.keyboard.press('Delete');
    await page.waitForTimeout(50);
    await page.keyboard.press('Delete');
    await page.waitForTimeout(100);

    // Should be "abe"
    const editorText = await page.textContent('.editable-area');
    expect(editorText).toContain('abe');
  });

  test('P0-05: Arrow key navigation', async ({ page }) => {
    // Type two lines
    await page.keyboard.type('line 1', { delay: 10 });
    await page.waitForTimeout(50);
    await page.keyboard.press('Enter');
    await page.waitForTimeout(50);
    await page.keyboard.type('line 2', { delay: 10 });
    await page.waitForTimeout(50);

    // Test Up Arrow - should move to line 1
    await page.keyboard.press('ArrowUp');
    await page.waitForTimeout(50);

    // Type something - should appear on line 1
    await page.keyboard.type('X', { delay: 10 });
    await page.waitForTimeout(100);

    const editorText = await page.textContent('.editable-area');
    expect(editorText).toMatch(/line 1.*X/);
  });

  test('P0-06: Mouse click positioning', async ({ page }) => {
    // Type several lines
    await page.keyboard.type('First line', { delay: 10 });
    await page.waitForTimeout(50);
    await page.keyboard.press('Enter');
    await page.waitForTimeout(50);
    await page.keyboard.type('Second line', { delay: 10 });
    await page.waitForTimeout(50);
    await page.keyboard.press('Enter');
    await page.waitForTimeout(50);
    await page.keyboard.type('Third line', { delay: 10 });
    await page.waitForTimeout(100);

    // Click somewhere in the middle (this is harder to test precisely)
    // For now, just verify clicking doesn't crash
    await page.click('.editable-area');
    await page.waitForTimeout(50);

    // Type a character - should insert at clicked position
    await page.keyboard.type('X', { delay: 10 });
    await page.waitForTimeout(100);

    const editorText = await page.textContent('.editable-area');
    expect(editorText).toContain('X');
  });
});
