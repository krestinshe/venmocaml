Caml1999N031����            /src/account.mli����  �  E  �  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����%venmo��.<command-line>A@A�A@F@@��A@@�A@G@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text��������?Representation of Venmo account��/src/account.mliA@@�A@d@@@@@@���A�    �&amount��Cfk�Cfq@@@@A@���)ocaml.docǐ������	3 The abstract type of values representing amounts. ��Drr�Dr j@@@@@@@��Cff@@�@���A�    �+transaction��'F l q�(F l |@@@@A@���␠�����	A The abstract type of values representing VenmOCaml transations. ��5G } }�6G } �@@@@@@@��8F l l@@�@���A�    �,notification��BI � ��CI � �@@@@A@���7��������	@The abstract type of values representing VenmOCaml notifications��PJ � ��QJ �@@@@@@@��SI � �@@�@���A�    �!t��]L#�^L$@@@@A@���R�������	4 The abstract type of values representing accounts. ��kM%%�lM%^@@@@@@@��nL@@�@������-InvalidAmount��xO`j�yO`w@�@������&string���O`{��O`�@@�@@@@@���O``@���{A�������	J Raised when a user attempts to perform an action with an invalid amount. ���P����P��@@@@@@@L@�@������/InvalidCurrency���R����R��@�@������&string���R����R��@@�@@@@@���R��@����h�������	K Raised when a user attempts to define an amount with an invalid currency. ���S����S�G@@@@@@@s@�@������.InvalidDeposit���UIS��UIa@�@������&string���UIe��UIk@@�@@@@@���UII@������������	� Placeholder until exchange rates are established. Raised when a user
    attempts to deposit a currency that is different from its balance. ���Vll��W��@@@@@@@�@�@������1InvalidWithdrawal���Y 
��Y @�@������&string���Y ��Y %@@�@@@@@���Y  @������������	� Placeholder until exchange rates are established. Raised when a user
    attempts to withdraw a currency that is different from its balance. ��	Z&&�
[o�@@@@@@@�@�@������1InvalidConversion��]���]��@�@�@@��]��@���Ӑ������	> Raised when a user attempts to complete an invalid conversion��&^���'^�@@@@@@@�@�@���Р)from_json��0` �1`)@��@������&Yojson%Basic!t��>`,�?`:@@�@@@��@����#int��I`>�J`A@@�@@@����!t��R`E�S`F@@�@@@�@@@�@@@@���J�������	m [from_json j] is the account that [j] represents. Requires: [j] is a valid
    JSON account representation. ��caGG�db��@@@@@@@��f`@�@���Р'to_json��od���pd��@��@����!t��yd���zd��@@�@@@������&Yojson%Basic!t���d����d��@@�@@@�@@@@���}C�������	4 [to_json acc] is the Yojson that represents [acc]. ���e����e�@@@@@@@���d��@�@���Р&create���g��g"@��@����#int���g%��g(@@�@@@��@����&string���g,��g2@@�@@@��@����&string���g6��g<@@�@@@���'balance����&string���gI��gO@@�@@@��@����&string���gS��gY@@�@@@����!t���g]��g^@@�@@@�@@@���g@@@@�(@@@�4@@@�@@@@@������������	� [create id username password ~balance:balance home_curr] is the account with
    unique identifier [id], username [username], password [password], balance
    [balance] (or 0. by default), home currency [home_curr], and is active. ���h__��j�L@@@@@@@���g@�@���Р(username��lNR�lNZ@��@����!t��lN]�lN^@@�@@@����&string��lNb�lNh@@�@@@�@@@@���Ԑ������	2 [username acc] is the username of account [acc]. ��'mii�(mi�@@@@@@@��*lNN@�@�����;琠�����	2 [password acc] is the password of account [acc]. ��:p���;p��@@@@@@���Р.check_password��Cr���Dr�@��@����&string��Mr��Nr�@@�@@@��@����!t��Xr��Yr�@@�@@@����$bool��ar��br�#@@�@@@�@@@�@@@@���Y�������	F[check_password str] returns true if the str equals account's password��rs$$�ss$o@@@@@@@��ur��@�@���Р'balance��~uqu�uq|@��@����!t���uq��uq�@@�@@@����&string���uq���uq�@@�@@@�@@@@����N�������	{ [balance acc] is the current balance of account [acc] with the understanding
    that a user can have a negative balance. ���v����w�@@@@@@@���uqq@�@���Р)is_active���y��y@��@����!t���y��y@@�@@@����$bool���y"��y&@@�@@@�@@@@����}�������	Y [is_active acc] returns true if the account is active; otherwise, it returns
    false. ���z''��{x�@@@@@@@���y@�@���Р*deactivate���}����}��@��@����!t���}����}��@@�@@@����!t���}����}��@@�@@@�@@@@������������	� [deactivate acc] returns a copy of [acc] that is inactive if [acc] is
    active, and does nothing if [acc] is already inactive. ���~��� �	&@@@@@@@��}��@�@���Р'display�� F	�	�� F	�
 @��@����!t�� F	�
� F	�
@@�@@@����&string�� F	�
� F	�
@@�@@@�@@@@���ې������	S [display acc] returns a string of the account, with its username, and
    balance.��. G

�/ H
Y
g@@@@@@@��1 F	�	�@�@���Р/display_history��: J
i
m�; J
i
|@��@����!t��D J
i
�E J
i
�@@�@@@����&string��M J
i
��N J
i
�@@�@@@�@@@@���D
�������	E [display acc] returns a string of the account's transaction history.��] K
�
��^ K
�
�@@@@@@@��` J
i
i@�@���Р'deposit��i M
�
��j M
�
�@��@����!t��s M
�
��t M
�
�@@�@@@��@����&string��~ M
�
�� M
�
�@@�@@@����!t��� M
�
��� M
�
�@@�@@@�@@@�@@@@���E�������	i [deposit acc amt] adds [amt] to the balance of the account [acc] with the
    precondition that amt > 0 ��� N
�
��� ODd@@@@@@@��� M
�
�@�@���Р(withdraw��� Qfj�� Qfr@��@����!t��� Qfu�� Qfv@@�@@@��@����&string��� Qfz�� Qf�@@�@@@����!t��� Qf��� Qf�@@�@@@�@@@�@@@@������������	p [withdraw acc amt] removes [amt] from the balance of the account [acc] with
    the precondition that amt > 0. ��� R���� S��@@@@@@@��� Qff@�@���Р0add_notification��� U��� U�@��@����!t��� U��� U�@@�@@@��@����,notification��� U��� U�%@@�@@@����$unit��� U�)�� U�-@@�@@@�@@@�@@@@������������	R [add_notification acc not] adds notification not into notification_inbox in
    t�� V..� W~�@@@@@@@�� U��@�@���Р+notif_clear�� Y��� Y��@��@����!t��$ Y���% Y��@@�@@@����$unit��- Y���. Y��@@�@@@�@@@@���$ꐠ�����	3 [notif_clear] clears the notification inbox of acc��= Z���> Z��@@@@@@@��@ Y��@�@���Р,make_request��I \���J \��@��@����!t��S \���T \��@@�@@@��@����&string��^ \���_ \��@@�@@@��@����&string��i \���j \�@@�@@@����,notification��r \�	�s \�@@�@@@�@@@�@@@�%@@@@���k1�������	d [make_request t payer amount] requests payer [amount] value of money and
    returns a notification��� ]�� ^c@@@@@@@��� \��@�@@