Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test18.c`
        Then the output should contain exactly:
        """
        Vous avez entre 30 et 50 ans.
        Soit vous êtes mineur, soit vous avez plus de 30 ans.
        Soit vous êtes mineur, soit vous avez plus de 30 ans.
        Soit vous avez entre 30 et 50 ans, soit vous avez plus de 60 ans.
        Vous n’êtes pas mineur.

        """
