class Person:
    def __init__(self, name="", father=None, mother=None):
        self.name = name
        self.father = father
        self.mother = mother

def parents(person):
    result = []
    if person is not None:
        if person.father is not None:
            result.append(person.father)
        if person.mother is not None:
            result.append(person.mother)
    return result

def grandparents(person):
    result = []
    for p in parents(person):
        p = parents(p)
        if p is not None:
            result.append(p)
    return result
