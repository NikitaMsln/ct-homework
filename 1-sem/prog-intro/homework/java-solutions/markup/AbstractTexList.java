package markup;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractTexList implements TexItem {
    private final List<ListItem> items;

    public AbstractTexList(List<ListItem> items) {
        this.items = new ArrayList<>(items);
    }

    protected void toTex(StringBuilder result, String begin, String end) {
        result.append(begin);
        for (ListItem item : items) {
            item.toTex(result);
        }
        result.append(end);
    }
}
