Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test5.c`
        Then the output should contain exactly:
        """
        0
        1
        2
        3
        4
        abcdefghijklmnopqrstuvwxyz

        """
