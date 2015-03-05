Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test2.c`
        Then the output should contain exactly:
        """
        Hello world!
        Hello            World!
        12
        42, 24: 12
        a\'
        a	b
        Helllo	World!"Bonjour
        \
        acb
        acb
        -42
        -3.141592

        """
