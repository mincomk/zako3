# HQ Features

- Emoji/Sticker Matching: Backend stores pHash of emoji or sticker when a global mapping is registered. When a bot see a new emoji ID, it calculates pHash of it and compares through pHash. And it automatically adds mapping of same emoji to the global scope.
