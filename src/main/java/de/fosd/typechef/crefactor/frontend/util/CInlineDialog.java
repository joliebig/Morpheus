package de.fosd.typechef.crefactor.frontend.util;

import javax.swing.*;
import java.awt.event.*;

public class CInlineDialog extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JCheckBox keepFunctionDecls;
    private boolean keepDeclartions = false;
    private boolean refactor = false;

    public CInlineDialog() {
        setContentPane(contentPane);
        setModal(true);
        getRootPane().setDefaultButton(buttonOK);

        buttonOK.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onOK();
            }
        });

        buttonCancel.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        });

        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                onCancel();
            }
        });

        contentPane.registerKeyboardAction(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                onCancel();
            }
        }, KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    }

    private void onOK() {
        keepDeclartions = keepFunctionDecls.isSelected();
        refactor = true;
        dispose();
    }

    private void onCancel() {
        dispose();
    }

    public boolean isRefactor() {
        return refactor;
    }

    public boolean isKeepDeclarations() {
        return keepDeclartions;
    }

}
