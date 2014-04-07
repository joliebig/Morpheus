package de.fosd.typechef.crefactor.frontend.actions.refactor;


import de.fosd.typechef.crefactor.Morpheus;
import de.fosd.typechef.crefactor.backend.engine.CExtractFunction;
import de.fosd.typechef.crefactor.backend.engine.CInlineFunction;
import de.fosd.typechef.crefactor.backend.engine.CRenameIdentifier;
import de.fosd.typechef.crefactor.evaluation_utils.Configuration;
import de.fosd.typechef.crefactor.frontend.util.CInlineDialog;
import de.fosd.typechef.crefactor.frontend.util.RefactorNameInputBox;
import de.fosd.typechef.parser.c.AST;
import de.fosd.typechef.parser.c.Id;
import de.fosd.typechef.parser.c.TranslationUnit;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import scala.collection.immutable.List;
import scala.util.Either;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

public class RefactorAction {

    private static Logger logger = LogManager.getLogger(RefactorAction.class);

    public static Action getExtractFunction(final Morpheus morpheus, final List<AST> selection) {

        return new AbstractAction() {

            {
                putValue(Action.NAME, Configuration.getInstance().getConfig("refactor.extractFunction"));
            }

            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                final RefactorNameInputBox box = new RefactorNameInputBox();
                box.createAndShowInputBox(Configuration.getInstance().getConfig("refactor.extractFunction"),
                        Configuration.getInstance().getConfig("refactor.extractFunction.name"),
                        Configuration.getInstance().getConfig("refactor.extractFunction.defaultFuncName"));
                if (box.getInput() == null) {
                    return;
                }

                try {
                    final ThreadMXBean tb = ManagementFactory.getThreadMXBean();
                    final long startTime = tb.getCurrentThreadCpuTime();
                    final Either<String, TranslationUnit> refactored =
                            CExtractFunction.extract(morpheus, selection, box.getInput());
                    if (refactored.isLeft()) {
                        JOptionPane.showMessageDialog(null,
                                Configuration.getInstance().getConfig("refactor.extractFunction.failed")
                                        + " " + refactored.left().get(),
                                Configuration.getInstance().getConfig("default.error"),
                                JOptionPane.ERROR_MESSAGE);
                    } else {
                        morpheus.update(refactored.right().get());
                    }
                    logger.info("Duration for transforming: "
                            + (tb.getCurrentThreadCpuTime() - startTime) / 1000000 + "ms");
                } catch (final AssertionError e) {
                    JOptionPane.showMessageDialog(null,
                            Configuration.getInstance().getConfig("refactor.extractFunction.failed")
                                    + " " + e.getMessage(),
                            Configuration.getInstance().getConfig("default.error"),
                            JOptionPane.ERROR_MESSAGE);

                } catch (final Exception e) {
                    e.printStackTrace();
                }

            }
        };
    }

    public static Action getInlineFunctionAction(final Morpheus morpheus, final Id id) {

        return new AbstractAction() {

            {
                putValue(Action.NAME, Configuration.getInstance().getConfig("refactor.inline")
                        + " " + id.name());
            }

            @Override
            public void actionPerformed(final ActionEvent actionEvent) {
                if (CInlineFunction.isFunctionCall(morpheus, id)) {
                    logger.info("InlineOnce");
                }
                final CInlineDialog dialog = new CInlineDialog();
                dialog.pack();
                dialog.setVisible(true);

                if (!dialog.isRefactor()) {
                    return;
                }

                try {
                    final ThreadMXBean tb = ManagementFactory.getThreadMXBean();
                    final long startTime = tb.getCurrentThreadCpuTime();
                    final Either<String, TranslationUnit> inlineResult = CInlineFunction.inline(morpheus, id,
                            dialog.isRename(), dialog.isKeepDeclarations());
                    logger.info("Duration for transforming: "
                            + ((tb.getCurrentThreadCpuTime() - startTime) / 1000000) + "ms");

                    if (inlineResult.isLeft()) {
                        JOptionPane.showMessageDialog(null,
                                Configuration.getInstance().getConfig("refactor.inline.failed")
                                        + " " + inlineResult.left().get(),
                                Configuration.getInstance().getConfig("default.error"),
                                JOptionPane.ERROR_MESSAGE);
                    } else {
                        morpheus.update(inlineResult.right().get());
                    }
                } catch (final AssertionError e) {
                    JOptionPane.showMessageDialog(null,
                            Configuration.getInstance().getConfig("refactor.inline.failed")
                                    + " " + e.getMessage(),
                            Configuration.getInstance().getConfig("default.error"),
                            JOptionPane.ERROR_MESSAGE);

                } catch (final Exception e) {
                    e.printStackTrace();
                }

            }
        };
    }

    public static Action getRenameAction(final Morpheus morpheus, final Id id) {
        return new AbstractAction() {

            {
                putValue(Action.NAME, Configuration.getInstance().getConfig("refactor.rename")
                        + " \"" + id.name() + "\"");
            }

            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                final RefactorNameInputBox box = new RefactorNameInputBox();
                box.createAndShowInputBox(Configuration.getInstance().getConfig("refactor.rename.name"),
                        Configuration.getInstance().getConfig("engine.rename.newName"), id.name());
                if (box.getInput() == null) {
                    return;
                }

                try {
                    final ThreadMXBean tb = ManagementFactory.getThreadMXBean();
                    final long time = tb.getCurrentThreadCpuTime();
                    final Either<String, TranslationUnit> refactored =
                            CRenameIdentifier.rename(id, box.getInput(), morpheus);
                    logger.info("Duration for transforming: "
                            + (tb.getCurrentThreadCpuTime() - time) / 1000000 + "ms");
                    if (refactored.isLeft()) {
                        JOptionPane.showMessageDialog(null,
                                Configuration.getInstance().getConfig("refactor.rename.failed")
                                        + " " + refactored.left().get(),
                                Configuration.getInstance().getConfig("default.error"),
                                JOptionPane.ERROR_MESSAGE);
                    } else {
                        morpheus.update(refactored.right().get());
                    }

                } catch (final AssertionError e) {
                    JOptionPane.showMessageDialog(null,
                            Configuration.getInstance().getConfig("refactor.rename.failed")
                                    + " " + e.getMessage(),
                            Configuration.getInstance().getConfig("default.error"),
                            JOptionPane.ERROR_MESSAGE);

                } catch (final Exception e) {
                    e.printStackTrace();
                }
            }
        };
    }
}