package markup;

import java.util.ArrayList;
import java.util.List;

public class ListItem{
    private List<TexItem> elements;

    public ListItem(List<TexItem> elements) {
        this.elements = new ArrayList<>(elements);
    }

    public void toTex(StringBuilder result) {
        result.append("\\item ");
        for (TexItem element : elements) {
            element.toTex(result);
        }
    }
}
