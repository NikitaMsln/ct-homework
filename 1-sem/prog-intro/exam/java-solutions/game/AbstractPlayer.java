package game;

abstract public class AbstractPlayer implements Player {
    final private String name;

    protected AbstractPlayer(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }
}
