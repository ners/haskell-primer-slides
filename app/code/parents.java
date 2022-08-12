public class Person {
    private String name;
    private Person father;
    private Person mother;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Person getFather() {
        return father;
    }

    public void setFather(Person father) {
        this.father = father;
    }

    public Person getMother() {
        return mother;
    }

    public void setMother(Person mother) {
        this.mother = mother;
    }

    public List<Person> getParents() {
        List<Person> parents = new List<Person>();
        if (this.father != null) {
            parents.add(this.father);
        }
        if (this.mother != null) {
            parents.add(this.mother);
        }
        return parents;
    }

    public List<Person> getGrandparents() {
        List<Person> grandparents = new List<Person>();
        for (Person p : getParents()) {
            grandparents.addAll(p.getParents());
        }
        return grandparents;
    }
}
