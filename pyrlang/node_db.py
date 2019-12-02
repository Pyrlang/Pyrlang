
class NodeDB:
    """
    A singleton class for keeping track of the nodes that created withing
    a python process.

    Even though it's considered a bit of an anti pattern to run several nodes
    in one python process, we created this db class to inhibit that behaviour
    by default but still give some options if you really know what you're doing.

    """
    __singleton = None

    def __new__(cls):
        if not cls.__singleton:
            cls.__singleton = super().__new__(cls)
        return cls.__singleton

    def __init__(self):
        self.__db = {}
        self.__active_node = None

    def _get_key(self, in_data):
        if type(in_data) == str:
            return in_data
        else:
            return in_data.node_name_

    def get_all(self):
        """
        return the dict of registered nodes
        """
        return self.__db

    def get(self, node_name=None):
        """
        Get the active node or the one specified
        """
        node_name = node_name or self.__active_node
        if not node_name:
            raise AttributeError("there is no active node")
        node_name = self._get_key(node_name)
        if node_name not in self.__db:
            msg = "there is no node {} registered".format(node_name)
            raise AttributeError(msg)
        return self.__db[node_name]

    def get_loop(self, node_name=None):
        """
        Get the event loop for the active node, or the one specified
        """
        return self.get(node_name).get_loop()

    def register(self, node):
        """
        registers and sets the node as the active one
        """
        self.__db[self._get_key(node)] = node
        self.activate(node)

    def activate(self, node):
        """
        set the node as active one.

        Can't be done if there already exists an active node, so that one
        needs to be deactivated first
        """
        if self.__active_node is not None:
            raise AttributeError("there is already an active node: "
                                 "{}".format(self.__active_node))
        node_name = self._get_key(node)
        if node_name not in self.__db:
            raise AttributeError("node {} not in database".format(node))
        self.__active_node = node_name

    def deactivate(self, node):
        """
        Remove the node as the active one

        This should typically not be done and exists for edge test cases and
        strange things.
        """
        key = self._get_key(node)
        if key != self.__active_node:
            raise AttributeError("{} is not the active node".format(node))
        self.__active_node = None

    def remove(self, node):
        """
        Remove the node from the db
        """
        key = self._get_key(node)
        if key == self.__active_node:
            self.deactivate(node)
        self.__db.pop(key)

