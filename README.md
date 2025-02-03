# Quarto Website for the Evolutionary Ecology Group at the University of Cambridge

This repository contains [Quarto](https://quarto.org) documents for our group website, which
is hosted at [https://evolecolgroup.github.io/website](https://evolecolgroup.github.io/website).

When you render the website in RStudio, a static HTML version is updated in the `docs` folder. This is then
served by GitHub Pages for the `main` branch.

## For new group members
To add your profile to the website, please follow these steps:
1) Clone the repository and create a new branch.
2) Go to the `people` folder in this repository.
3) Make a copy of the template profile file "_template_profile.qmd", and rename it by setting your name as the file name (i.e. `firstname_lastname.qmd`).
4) Edit the file to include your details.
5) Add a picture of yourself in the same folder, again using your name as the file name (e.g. `firstname_lastname.png`). The picture should be square, ideally 250px by 250px.
6) If you don't already, consider having a Google Scholar profile, and include the link in your profile.
7) Render the quarto file to check that it looks good. Your card should be automatically added to the people page.
8) Commit your changes and submit a pull request.


## To add a resource
To add a resource (software, data, etc.) to the website, please follow these steps:
1) Clone the repository and create a new branch.
2) Go to the `resources` folder in this repository.
3) Add an item to "resources.yaml", following the existing format.
4) Add an appropriate image associated with the resource to the `img` folder. The image should be 240px by 276px (the same size as logos for R packages).
5) Render the resources page to check that it looks good.
6) Commit your changes and submit a pull request.


## To update the group publications (not functional yet!)
We need to create a system where publications can be added to the group website.
Currently, we simply fetch the full set of Andrea's publications from Google Scholar.
Only Andrea's name is highlighted, we need to create a system to highlight all group members' names.

## Notes
The website uses two extensions, `fontawesome` and `academicicons`. These allow the inclusion of
additional, non-standard icons using shortcodes.
