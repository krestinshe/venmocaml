Caml1999N031����            -src/state.mli����  �  �  �  @�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����%venmo��.<command-line>A@A�A@F@@��A@@�A@G@@@@�@@�������@�@@@�@@�@@@@�@@@�@�������*ocaml.text��������	� Representation of dynamic Venmo state.

    This module represents the state of a VenmOCaml system since its start,
    including the accounts that have been created, a history of transactions,
    and functions that cause the state to change. ��-src/state.mliA@@�E � �@@@@@@���A�    �+transaction��G ��G �@@@@A@���)ocaml.docǐ������	C The abstract type of values representing a VenmOCaml transaction. ��H�HU@@@@@@@��G � �@@�@���A�    �!t��'JW\�(JW]@@@@A@���␠�����	? The abstract type of values representing the VenmOCaml state. ��5K^^�6K^�@@@@@@@��8JWW@@�@������/InvalidUsername��BM���CM��@�@������&string��MM���NM��@@�@@@@@��QM��@���E�������	} Raised when the user attempts to create an account with a username that is
    already associated with an existing account. ��^N���_OJ@@@@@@@@�@������1IncorrectPassword��iQLV�jQLg@�@�@@��nQLL@���b(�������	D Raised when the user enters the incorrect password for an account. ��{Rhh�|Rh�@@@@@@@3@�@���Р*init_state���T����T��@����!t���T����T��@@�@@@@����I�������	� [init_state] is the initial state of the Venmo system. The user is not
    logged into any account and there have been no accounts created or
    transactions made. ���U����WXq@@@@@@@���T��@�@���Р.check_username���Ysw��Ys�@��@����!t���Ys���Ys�@@�@@@��@����&string���Ys���Ys�@@�@@@����$unit���Ys���Ys�@@�@@@�@@@�@@@@������������	� [check_username st un] raises [InvalidUsername un] if [un] is already
    associated with an account in the current state, and returns [()] otherwise. ���Z����[�9@@@@@@@���Yss@�@���Р/current_account���];?��];N@��@����!t���];Q��];R@@�@@@����&option���];`��];f@������'Account!t��];V�];_@@�@@@@�@@@�@@@@������������	_ [current_account st] is the account that the user is currently logged into
    in state [st]. ��^gg�_��@@@@@@@��];;@� @���Р(accounts��a���a��@��@����!t��(a���)a��@@�@@@����%array��1a���2a��@������'Account!t��<a���=a��@@�@@@@�@@@�@@@@���4��������	] [accounts st] is a set-like array of the accounts that have been created in
    state [st]. ��Mb���NcAS@@@@@@@��Pa��@� @���Р,transactions��YeUY�ZeUe@��@����!t��ceUh�deUi@@�@@@����$list��leUy�meU}@�����+transaction��ueUm�veUx@@�@@@@�@@@�@@@@���m3�������	� [transactions st] is a list of the transactions that have been made in state
    [st], ordered from least recent to most recent. ���f~~��g�@@@@@@@���eUU@�@���Р+add_account���i��i@��@����!t���i��i@@�@@@��@�����'Account!t���i��i'@@�@@@����$unit���i+��i/@@�@@@�@@@�@@@@����p�������	W [add_account st acc] adds [acc] to the list of accounts in [st] and returns
    [()]. ���j00��k��@@@@@@@���i@�@���Р.delete_account���m����m��@��@����!t���m����m��@@�@@@��@����#int���m����m��@@�@@@����$unit���m����m��@@�@@@�@@@�@@@@������������	e [delete_account st acc] makes the account identified by [id] inactive in
    [st] and returns [()]. ���n����o@@@@@@@��m��@�@���Р,make_deposit��
q $�q 0@��@����!t��q 3�q 4@@�@@@��@����&string��q 8� q >@@�@@@��@����&string��*q B�+q H@@�@@@����$unit��3q L�4q P@@�@@@�@@@�@@@�%@@@@���,򐠠����	Y [make_dposit st un p] increases the balance of the account with username
    [un] by [p]��ErQQ�Fs��@@@@@@@��Hq  @�@���Р,make_payment��Qu���Ru��@��@����!t��[u���\u��@@�@@@��@����#int��fu���gu��@@�@@@��@����#int��qu���ru��@@�@@@��@����&string��|u���}u��@@�@@@����$unit���u����u��@@�@@@�@@@�@@@�%@@@�1@@@@���E�������	�[make_payment st
  paying_acc_id paid_acc_id p] adds [p] to the balance of
   account identified by [paid_acc_id] and removes [p] from the balance of the
   account identified by [paying_acc_id]���v����y��@@@@@@@���u��@�@���Р,login_system���{����{��@��@����!t���{����{��@@�@@@��@����&string���{����{��@@�@@@��@����&string���{����{��@@�@@@����$unit���{����{��@@�@@@�@@@�@@@�%@@@@������������	� [login_system st un pass] changes current account of the state if the
    username and password input matches one of the accounts in the state���|����}	*	t@@@@@@@���{��@�@���Р&logout���	v	z��	v	�@��@����!t���	v	���	v	�@@�@@@����$unit���	v	���	v	�@@�@@@�@@@@������������	5 [logout st] removes the current account of the state�� @	�	�� @	�	�@@@@@@@��	v	v@�@���Р'to_file�� B	�	�� B	�	�@��@����!t��$ B	�	��% B	�	�@@�@@@����$unit��- B	�	��. B	�	�@@�@@@�@@@@���$ꐠ�����	^ [to_file st] converts the state [st] to a JSON object and stores it in
    "data/data.json". ��= C	�	��> D
,
D@@@@@@@��@ B	�	�@�@@