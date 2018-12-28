# GitHub projects dependencies

This is a project organization assistant initially aimed to produce artifacts, such as dependency tree diagrams, which could potentially be included in the issues and PR descriptions of software projects hosed at GitHub.

:bug: :warning: At this point, this is a toy project and it is not intended to be bug free, nor safe or even stable.

## Features

  - [x] Command line dependency trees generation in `png` format.
  - [ ] Post generated dependency trees as part of issue comments automatically.


## Command line dependency trees generation

This feature allows generating issues dependecy directed trees where a node points to another when the former requires the latter to be completed.

For this, the assistant generates a dependency graph ...

- Given a meta issue where all the project issues are listed, that is: Present as URLs in the meta issue description text. e.g: https://github.com/pfcoperez/gh-project-dependencies/issues/1

- Having each issue a pointer to the metaissue it belongs to as an URL preceded by the words _part of_.
- And Given that it also list the issues it depends on using their URLs preceded by the words _blocked by_.

e.g:

https://github.com/pfcoperez/gh-project-dependencies/issues/5


![image](https://user-images.githubusercontent.com/273379/50519798-eabe9a00-0abc-11e9-897f-2c60a5e1130c.png)


The image of the example can be generated after having checked out this project source code by typing:

```bash
TOKEN=$(cat ./experiments.token) stack run gh-project-dependencies pfcoperez gh-project-dependencies 1 5
```

Where `./experiments.token` contains the personal security token generated for this application using the GitHub settings:

_Settings -> Developer settings -> Personal access tokens -> Generate new token._





