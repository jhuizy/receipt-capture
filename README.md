# Receipt Manager

A simple web application for managing receipts, built with haskell, tesseract, yesod and nix

#  Features

- OCR (via tesseract)
- Nothing else

# Development

You can develop this project by spinning up a nix shell (note: requires nix flakes)

```bash
nix develop
```

The project can also be built manually with cabal, but it also requires tesseract (version 4) to be available on $PATH. 

# Building

Building is also as simple as running the nix build command

```bash
nix build
```

This creates an executable in `./result/bin/receipt-capture-exe`

# Deployment

TODO...
