package de.fosd.typechef.crefactor.frontend;

import de.fosd.typechef.crefactor.Morpheus;
import de.fosd.typechef.crefactor.backend.codeselection.CExtractSelection;
import de.fosd.typechef.crefactor.backend.codeselection.CInlineSelection;
import de.fosd.typechef.crefactor.backend.codeselection.CRenameSelection;
import de.fosd.typechef.crefactor.backend.engine.CInlineFunction;
import de.fosd.typechef.crefactor.evaluation_utils.Configuration;
import de.fosd.typechef.crefactor.frontend.actions.refactor.CRefactorAction;
import de.fosd.typechef.crefactor.frontend.util.CodeSelection;
import de.fosd.typechef.parser.c.AST;
import de.fosd.typechef.parser.c.Id;
import scala.collection.Iterator;
import scala.collection.immutable.List;

import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

/**
 * Implements an ActionListener in order to display valid and possible refactorings in the context menu.
 *
 * @author Andreas Janker
 */
public class RefactorMenu implements MenuListener {

    /**
     * Reference to the current editor instance.
     */
    private Editor editor;

    /**
     * Reference to the current menu instance.
     */
    private JMenu menu;

    /**
     * Generates a new menulistener instance for a editor and a menu.
     *
     * @param editor the used editor instance.
     * @param menu   the menu
     */
    public RefactorMenu(final Editor editor, final JMenu menu) {
        this.editor = editor;
        this.menu = menu;
    }

    @Override
    public void menuSelected(MenuEvent menuEvent) {
        this.menu.removeAll();
        final CodeSelection codeSelection = new CodeSelection(editor);
        final Morpheus morpheus = this.editor.getMorpheus();

        /**
         * Refactor Renaming
         */
        final List<Id> availableIds = CRenameSelection.getAvailableIdentifiers(morpheus, codeSelection);
        if (!availableIds.isEmpty()) {
            final JMenu rename = new JMenu(Configuration.getInstance().getConfig("refactor.rename.name"));
            this.menu.add(rename);
            addRenamingsToMenu(availableIds, rename);

            final List<AST> extractSelection = CExtractSelection.getSelectedElements(morpheus, codeSelection);

            if (!extractSelection.isEmpty()) {
                final JMenuItem extract = new JMenuItem(CRefactorAction.getExtractFunction(morpheus, extractSelection));
                this.menu.add(extract);
            }

            /**
             * Inline Function
             */
            final List<Id> availableFuncIDs = CInlineSelection.getAvailableIdentifiers(morpheus, codeSelection);
            if (!availableFuncIDs.isEmpty()) {
                final JMenu inline = new JMenu(Configuration.getInstance().getConfig("refactor.inline.name"));
                this.menu.add(inline);
                addInliningToMenu(availableFuncIDs, inline);
            }
        }
    }

    @Override
    public void menuDeselected(MenuEvent menuEvent) {
        // Just does nothing - not implemented
    }

    @Override
    public void menuCanceled(MenuEvent menuEvent) {
        // Just does nothing - not implemented
    }

    /**
     * Add possible renamings to menu.
     *
     * @param selectedIDs selected ids to rename
     * @param menu        the menu where to add the rename entries
     */
    private void addRenamingsToMenu(final List<Id> selectedIDs, final JMenu menu) {
        final Iterator<Id> it = selectedIDs.iterator();
        while (it.hasNext()) {
            menu.add(CRefactorAction.getRenameAction(this.editor.getMorpheus(), it.next()));
        }
    }

    private void addInliningToMenu(final List<Id> selectedIDs, final JMenu menu) {
        final Iterator<Id> it = selectedIDs.iterator();
        while (it.hasNext()) {
            menu.add(CRefactorAction.getInlineFunctionAction(this.editor.getMorpheus(), it.next()));
        }
    }
}
