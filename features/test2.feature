Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test2.c`
        Then the output should contain exactly:
        """
        Hello world!
        Hello            World!
        12
        a\'
        a	b
        Helllo	World!"Bonjour
        \

        """