# Critical Rules for Working on Lexicon

## NEVER Run Compilation Commands

**DO NOT** run any of these commands:
- `npm run build`
- `npm run compile`
- `shadow-cljs compile`
- `shadow-cljs release`
- `npx shadow-cljs compile`
- Any other compilation/build commands

**WHY:** The user has watch processes running for both app and tests. Running compilation:
1. Hides the results from the user
2. You have a bad habit of trivializing warnings as not important
3. The user is "fed up of repeating" this

**WHAT TO DO INSTEAD:**
- Make your code changes
- Tell the user you've made the changes
- Wait for the user to report test results from their watch process
- The user will tell you about failures/warnings

## Test Philosophy

- **NEVER** change tests to match broken code
- **NEVER** "relax" tests to hide bugs
- **NEVER** create "artificial closure" by making problems go away
- If a test fails, fix the CODE, not the TEST
- The user wants real problems exposed, even if tests fail for weeks
- "I don't want to make the problem go away and hide real problems that will bite me later"

## Development Workflow

1. User has shadow-cljs watch running (both app and test)
2. You make code changes
3. Watch automatically recompiles
4. User reports failures
5. You fix the actual bugs, not the tests
