package markup;

public interface MarkdownElement extends TexElement{
    void toMarkdown(StringBuilder result);
}
