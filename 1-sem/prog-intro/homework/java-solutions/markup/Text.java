package markup;

public class Text implements ParagraphElement{
    private final String text;

    public Text(String text) {
        this.text = text;
    }

    @Override
    public void toMarkdown(StringBuilder result) {
        result.append(text);
    }

    @Override
    public void toTex(StringBuilder result) {
        result.append(text);
    }
}
