Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test4.c`
        Then the output should contain exactly:
        """
        Vous êtes mineur.
        Vous avez 5 ans.
        Vous avez au maximum 50 ans.
        Vous avez moins de 30 ans.
        Vous n’avez pas 18 ans.

        """
