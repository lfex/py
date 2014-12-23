from erlport.erlterms import List as ErlPortList


class List(ErlPortList):

    def __init__(self, data):
        if isinstance(data, str):
            data = self.convert_string(data)
        super().__init__(data)

    @staticmethod
    def convert_string(string):
        return [ord(x) for x in string]
