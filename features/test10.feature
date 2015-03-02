Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test10.c`
        Then the output should contain exactly:
        """
        variable == 10
        variable != 12
        not variable != 10
        variable < 20
        variable <= 20
        variable > 2
        variable >= 2
        2 < variable
        Hello World!
        string[0] == 'H'
        'H' == string[0]

        """
