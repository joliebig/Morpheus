package de.fosd.typechef.crefactor.frontend.util;

import javax.swing.*;
import java.awt.event.*;

public class CInlineDialog extends JDialog {
    private JPanel contentPane;
    private JButton buttonOK;
    private JButton buttonCancel;
    private JCheckBox renameIDs;
    private boolean rename = false;
    private boolean once = false;
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
        rename = renameIDs.isSelected();
        refactor = true;
        dispose();
    }

    private void onCancel() {
        dispose();
    }

    public boolean isRefactor() {
        return refactor;
    }

    public boolean isOnce() {
        return once;
    }

    public boolean isRename() {
        return rename;
    }

    public static void main(String[] args) {
        CInlineDialog dialog = new CInlineDialog();
        dialog.pack();
        dialog.setVisible(true);
        System.exit(0);
    }
}
