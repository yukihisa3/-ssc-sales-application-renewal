****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　発注入力　　　　　　　　　　　　　*
*    作成日／更新日　　　：　00/05/08                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注ファイルの登録・修正・削除　　*
*                            照会を行う　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0010I.
 AUTHOR.                NAV. Y.Y.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
*
*---<<  仕入先マスタ  >>---*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STATUS.
*
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-STATUS.
*
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*
*---<<  発注ヘッダファイル  >>---*
     SELECT   HACHEDF   ASSIGN    TO        DA-01-VI-HACHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   HAH-F02
                        FILE      STATUS    IS   HAH-STATUS.
*
*---<<  発注明細ファイル  >>---*
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   HAM-F02
                                                 HAM-F03
                        FILE      STATUS    IS   HAM-STATUS.
*
*---<<  入庫ファイル  >>---*
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYK-F02
                                                 NYK-F03
                                                 NYK-F04
                                                 NYK-F05
                        FILE      STATUS    IS   NYK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FHA00101  OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  仕入先マスタ  >>---*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF        XFDLIB
              JOINING   SHI       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  商品コード変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  発注ヘッダファイル  >>---*
 FD  HACHEDF.
     COPY     HACHEDF   OF        XFDLIB
              JOINING   HAH       PREFIX.
*---<<  発注明細ファイル  >>---*
 FD  HACMEIF.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   HAM       PREFIX.
*---<<  入庫ファイル  >>---*
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYK       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  TEN-STATUS          PIC  X(02).
     02  SHI-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  SHO-STATUS          PIC  X(02).
     02  ZAI-STATUS          PIC  X(02).
     02  HAH-STATUS          PIC  X(02).
     02  HAM-STATUS          PIC  X(02).
     02  NYK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  EOF-FLG             PIC  X(01)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
 01  UPDT-FLG-AREA.
     02  UPDT-FLG            PIC  X(01)  OCCURS 6.
****  処理スイッチ  ****
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01).
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
     02  IXC                 PIC  9(02).
     02  IXD                 PIC  9(02).
****  システム日付  ****
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE2                PIC  9(08).
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  TORICD-FLG              PIC  9(01)          VALUE  ZERO.
 01  SAV-AREA.
     03  SAV-SOKO            PIC  X(02).
     03  SAV-TORICD          PIC  9(08).
     03  SAV-NOUKI           PIC  9(08).
     03  SAV-DATA            OCCURS 6.
         05  SAV-SHOCD       PIC  X(08).
         05  SAV-HINTN1      PIC  X(05).
         05  SAV-HINTN2      PIC  X(02).
         05  SAV-HINTN3      PIC  X(01).
         05  SAV-SURYO       PIC S9(09)V99.
         05  SAV-TANA        PIC  X(06).
*
****  チェック用    ****
 01  CHK-YMD                 PIC  9(08).
 01  CHK-SURYO1              PIC S9(09)V99.
 01  CHK-SURYO2              PIC S9(09)V99.
****  作業用    ****
 01  WK-SURYO                PIC S9(09)V99.
 01  WK-KIN                  PIC S9(09).
 01  WK-TANA                 PIC  X(06).
 01  SV-HACNO1               PIC  9(07)         VALUE ZERO.
 01  SV-DSPREC               PIC  X(2000).
 01  WK-LSIME                PIC  9(06).
 01  WK-LSIMER               REDEFINES  WK-LSIME.
     03  WK-LSIMER1          PIC  9(04).
     03  WK-LSIMER2          PIC  9(02).
 01  WK-NOUHIN               PIC  9(08)    VALUE ZERO.
 01  WK-NOUHIN-R  REDEFINES  WK-NOUHIN.
     03  WK-NOUHIN1          PIC  9(06).
     03  WK-NOUHIN2          PIC  9(02).
*支払締日チェック用
 01  WK-CHK1-DT.
     03  WK-CHK1-DT1         PIC  9(02)   VALUE  ZERO.
     03  WK-CHK1-DT2         PIC  9(02)   VALUE  ZERO.
 01  WK-CHK2-DT.
     03  WK-CHK2-DT1         PIC  9(02)   VALUE  ZERO.
     03  WK-CHK2-DT2         PIC  9(02)   VALUE  ZERO.
*支払締日変換用
 01  WK-SSIME.
     03  WK-SSIME-YYMM       PIC  9(04)   VALUE  ZERO.
     03  WK-SSIME-DD         PIC  9(02)   VALUE  ZERO.
***   部門コード退避
 01  WK-BUMON                PIC  9(04)    VALUE ZERO.
 01  WK-BUMON-CD.
     03  WK-BUMON-1          PIC  9(03)    VALUE ZERO.
     03  WK-BUMON-2          PIC  9(01)    VALUE ZERO.
****  チェックデジット算出サブルーチン作業用    ****
 01  LINK-AREA.
     03  LI-KBN              PIC  9(01).
     03  LI-KETA             PIC  9(01).
     03  LI-START            PIC  9(09).
     03  LI-END              PIC  9(09).
     03  LI-DENNO            PIC  9(09).
     03  LO-ERR              PIC  9(01).
     03  LO-NEXT             PIC  9(09).
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
     02  PMSG04            PIC N(07) VALUE
             NC"_取消　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0010I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"処理区分が違います".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"入力項目に間違いがあります".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前レコードがありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次レコードがありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"使用出来ない伝票区分が入力されました。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"発注日を入力して下さい".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"日付の入力に誤りがあります".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"納期は発注日以降を入力して下さい".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"担当を入力して下さい".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"場所を入力して下さい".
     02  MSG-ERR16           PIC  N(20)  VALUE
             NC"倉庫マスタに存在しません".
     02  MSG-ERR17           PIC  N(20)  VALUE
             NC"送料区分に誤りがあります".
     02  MSG-ERR18           PIC  N(20)  VALUE
             NC"送料を入力して下さい".
     02  MSG-ERR19           PIC  N(20)  VALUE
             NC"送料入力時は、送料区分を入力して下さい".
     02  MSG-ERR20           PIC  N(20)  VALUE
             NC"仕入先マスタに存在しません".
     02  MSG-ERR21           PIC  N(20)  VALUE
             NC"請求区分に誤りがあります".
     02  MSG-ERR22           PIC  N(20)  VALUE
             NC"税区分に誤りがあります".
     02  MSG-ERR23           PIC  N(20)  VALUE
             NC"得意先を入力して下さい".
     02  MSG-ERR24           PIC  N(20)  VALUE
             NC"得意先マスタに存在しません".
     02  MSG-ERR25           PIC  N(20)  VALUE
             NC"仕入先に対する得意先に誤りがあります".
     02  MSG-ERR26           PIC  N(20)  VALUE
             NC"量販店伝票_に誤りがあります".
     02  MSG-ERR27           PIC  N(20)  VALUE
             NC"店舗マスタに存在しません".
     02  MSG-ERR28           PIC  N(20)  VALUE
             NC"納入先に対する得意先が未入力です".
     02  MSG-ERR29           PIC  N(20)  VALUE
             NC"条件マスタに登録されてません".
     02  MSG-ERR30           PIC  N(20)  VALUE
             NC"商品コードを入力して下さい".
     02  MSG-ERR31           PIC  N(20)  VALUE
             NC"商品名称マスタに登録されてません".
     02  MSG-ERR32           PIC  N(20)  VALUE
             NC"単価区分に誤りがあります".
     02  MSG-ERR33           PIC  N(20)  VALUE
             NC"単価は入力できません".
     02  MSG-ERR34           PIC  N(20)  VALUE
             NC"摘要を入力して下さい".
     02  MSG-ERR35           PIC  N(20)  VALUE
             NC"明細を入力して下さい".
     02  MSG-ERR36           PIC  N(20)  VALUE
             NC"伝票区分に誤りがあります".
     02  MSG-ERR37           PIC  N(20)  VALUE
             NC"計上フラグに誤りがあります".
     02  MSG-ERR38           PIC  N(20)  VALUE
             NC"伝区＝５０（仕入）の時以外入力できません".
     02  MSG-ERR39           PIC  N(20)  VALUE
             NC"送先名入力時は、送先区分を１にして下さい".
     02  MSG-ERR40           PIC  N(20)  VALUE
             NC"送先名が入力されてません".
     02  MSG-ERR41           PIC  N(20)  VALUE
             NC"送先区分に誤りがあります".
     02  MSG-ERR42           PIC  N(20)  VALUE
             NC"直送以外は、送先名は入力できません".
     02  MSG-ERR43           PIC  N(20)  VALUE
             NC"支払締日を入力して下さい".
     02  MSG-ERR44           PIC  N(20)  VALUE
             NC"支払締日に誤りがあります".
     02  MSG-ERR45           PIC  N(20)  VALUE
             NC"指定された仕入先は他部門の仕入先です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 46  TIMES.
*８桁変換後セット
 01  NEW-HACYMD            PIC 9(08)  VALUE  ZERO.
 01  NEW-NOUKI             PIC 9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE     "HTOKMS "        TO   ERR-FL-ID.
     MOVE      TOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE     "HTENMS "        TO   ERR-FL-ID.
     MOVE      TEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSHIMS.
     MOVE     "ZSHIMS "        TO   ERR-FL-ID.
     MOVE      SHI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS "        TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE     "HSHOTBL"        TO   ERR-FL-ID.
     MOVE      SHO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIF.
     MOVE     "ZAMZAIF"        TO   ERR-FL-ID.
     MOVE      ZAI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HACHEDF.
     MOVE     "HACHEDF"        TO   ERR-FL-ID.
     MOVE      HAH-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HACMEIF.
     MOVE     "HACMEIF"        TO   ERR-FL-ID.
     MOVE      HAM-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NYKFILF.
     MOVE     "NYKFILF"        TO   ERR-FL-ID.
     MOVE      NYK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SHA0010I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SHA0010I-END.
     EXIT.
************************************************************
*      ■０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF      HJYOKEN   ZAMZAIF
                        NYKFILF   HACHEDF   HACMEIF.
     OPEN     INPUT     HTOKMS    HTENMS    ZSHIMS    ZSOKMS
                        HMEIMS    HSHOTBL.
*----<< ﾌﾞﾓﾝｺｰﾄﾞ ｼｭﾄｸ >>-*
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
             MOVE     WK-BUMON    TO   WK-BUMON-CD
     END-READ.
*    在庫締日取得
     MOVE     99             TO   JYO-F01.
     MOVE     "ZAI"          TO   JYO-F02.
     PERFORM  JYO-READ-SUB
     IF  INVALID-FLG   =     ZERO
         COMPUTE WK-LSIME    =    JYO-F05  +  1
         IF   WK-LSIMER2     >    12
              MOVE  1        TO   WK-LSIMER2
              ADD   1        TO   WK-LSIMER1
         END-IF
     END-IF.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   TOKHAN.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FHA00101.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-SYORI.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
 INIT-END.
     EXIT.
************************************************************
*      ■０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   SYORI-SUB
         WHEN      "2"  PERFORM   HACNO-SUB
         WHEN      "3"  PERFORM   HEAD-SUB
         WHEN      "4"  PERFORM   BODY-SUB
         WHEN      "5"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       処理区分入力              MAIN-FLG=1      *
*==========================================================*
 SYORI-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "SYORI"         TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   SPACE   TO   FHA00101
                        PERFORM             EDIT-SET-SYORI
                        PERFORM             EDIT-SET-HEAD
                        PERFORM             EDIT-SET-BODY
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   SYORICHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   SYORIK  TO   WK-SYORI
                            IF  SYORIK     =    1
                                PERFORM    HACNO-SET-SUB
                                PERFORM    HACYMD-SET-SUB
                                MOVE   "3"     TO   MAIN-FLG
                            ELSE
                                MOVE   "2"     TO   MAIN-FLG
                            END-IF
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 SYORI-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1     処理区分の入力チェック                    *
*----------------------------------------------------------*
 SYORICHK-SUB             SECTION.
     PERFORM         EDIT-SET-SYORI.
     MOVE     ZERO           TO   ERR-MSG-CD.
*処理区分 CHK
     IF  SYORIK    NOT  NUMERIC
         MOVE   ZERO         TO   SYORIK
     END-IF.
     EVALUATE   SYORIK
         WHEN   1
             MOVE   NC"登録"     TO   SYORIM
         WHEN   2
             MOVE   NC"修正"     TO   SYORIM
         WHEN   3
             MOVE   NC"削除"     TO   SYORIM
         WHEN   9
             MOVE   NC"照会"     TO   SYORIM
         WHEN   OTHER
             MOVE   02           TO   ERR-MSG-CD
             MOVE   "R"          TO   EDIT-OPTION  OF  SYORIK
             MOVE   "C"          TO   EDIT-CURSOR  OF  SYORIK
     END-EVALUATE.
     MOVE    ZERO           TO   SV-HACNO1.
*
 SYORICHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.2     発注_自動採番                            *
*----------------------------------------------------------*
 HACNO-SET-SUB          SECTION.
     MOVE     60             TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     PERFORM         JYO-READ-SUB.
*****2007/10/29 最大数を５桁に変更
*****IF       JYO-F04  =     9999999
     IF       JYO-F04  =     99999
         MOVE      ZERO      TO   JYO-F04
     END-IF.
     ADD      1              TO   JYO-F04.
     IF       JYO-F04  =   ZERO
         MOVE      1         TO   JYO-F04
     END-IF.
     MOVE     JYO-F04        TO   HACNO1.
     IF  INVALID-FLG   =   ZERO
         REWRITE                  JYO-REC
     END-IF.
*    採番発注_存在チェック
     PERFORM  HAH-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         GO   TO   HACNO-SET-SUB
     ELSE
         MOVE      ZERO      TO   INVALID-FLG
     END-IF.
 HACNO-SET-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.3     発注日セット                              *
*----------------------------------------------------------*
 HACYMD-SET-SUB           SECTION.
     MOVE     WK-DATE        TO   HACYMD.
     MOVE     ZERO           TO   KEIJYO.
     MOVE     ZERO           TO   SEIKYU.
     MOVE     ZERO           TO   ZEIKU.
 HACYMD-SET-END.
     EXIT.
*==========================================================*
*      2.2       発注_入力                MAIN-FLG=2      *
*==========================================================*
 HACNO-SUB           SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HACNO"         TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   HAC-BACK-SUB
         WHEN   "F012"
                        PERFORM   HAC-NEXT-SUB
         WHEN   "E000"
                        PERFORM   HACNOCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   HACDSP-SUB
                            IF   WK-SYORI  =    3  OR  9
                                 MOVE   "5"     TO   MAIN-FLG
                            ELSE
                                 MOVE   "3"     TO   MAIN-FLG
                            END-IF
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 HACNO-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     発注_の入力チェック                      *
*----------------------------------------------------------*
 HACNOCHK-SUB            SECTION.
     PERFORM         EDIT-SET-HEAD.
     MOVE       ZERO    TO   ERR-MSG-CD.
****  発注_のチェック  ****
     IF  HACNO1         NUMERIC
         MOVE    HACNO1         TO   SV-HACNO1
*
         MOVE    SPACE          TO   READ-FLG
*        発注ヘッダＲＥＡＤ
         PERFORM   HAH-READ-SUB
         IF   INVALID-FLG    =    ZERO
              PERFORM   HAM-START-SUB
              IF   EOF-FLG   =    "1"
                   MOVE     "1"   TO   READ-FLG
                   GO   TO   HACNOCHK-010
              END-IF
              PERFORM   HAM-READ-SUB
              IF   EOF-FLG   =    "1"
                   MOVE     "1"   TO   READ-FLG
                   GO   TO   HACNOCHK-010
              END-IF
              PERFORM   UNTIL    EOF-FLG   =    "1"   OR
                                 READ-FLG  =    "1"   OR
                                 HAM-F02   NOT = HACNO1
                   IF   HAM-F10   >   ZERO
                        MOVE     "1"   TO   READ-FLG
                   END-IF
                   IF   HAM-F01 = 50 OR 60
                        IF   HAM-F05  NOT = ZERO
                             MOVE     "1"   TO   READ-FLG
                        END-IF
                   ELSE
                        IF   HAH-F22  NOT = ZERO
                             MOVE     "1"   TO   READ-FLG
                        END-IF
                   END-IF
*                  取消フラグ
                   IF   HAH-F24   NOT =     ZERO
                        MOVE     "1"   TO   READ-FLG
                   END-IF
                   PERFORM   HAM-READ-SUB
              END-PERFORM
              IF   READ-FLG   NOT =    "1"
                   MOVE HAH-F01   TO   DENKU
              END-IF
         END-IF
     ELSE
         MOVE    "1"        TO   READ-FLG
     END-IF.
*
 HACNOCHK-010.
*
     IF (INVALID-FLG     =  "1" )  OR
        (READ-FLG        =  "1" )
         IF  ERR-MSG-CD  =  ZERO
             MOVE  07       TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  HACNO1
             MOVE   "C"     TO   EDIT-CURSOR  OF  HACNO1
         END-IF
     ELSE
         MOVE      01        TO   JYO-F01
         MOVE      DENKU     TO   JYO-F02
         PERFORM             JYO-READ-SUB
         IF  INVALID-FLG   =   ZERO
             MOVE       JYO-F03   TO   DENKUM
             REWRITE    JYO-REC
         ELSE
             MOVE       SPACE     TO   DENKUM
             MOVE  03        TO   ERR-MSG-CD
             MOVE  "R"       TO   EDIT-OPTION  OF  DENKU
             MOVE  "C"       TO   EDIT-CURSOR  OF  DENKU
         END-IF
     END-IF.
 HACNOCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.3     発注ファイル前レコード読込／表示          *
*----------------------------------------------------------*
 HAC-BACK-SUB             SECTION.
     MOVE    SPACE        TO   READ-FLG.
     PERFORM    HAC-BACKSTART-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAH-NEXTREAD-SUB
         IF  EOF-FLG  =  ZERO
             MOVE   DSP-REC    TO   SV-DSPREC
             PERFORM    HEADDEL-SUB
             PERFORM    BODYDEL-SUB
             MOVE   HAH-F01    TO  DENKU
             MOVE   HAH-F02    TO  HACNO1    SV-HACNO1
             PERFORM    HAM-START-SUB
             PERFORM    HAC-SET-SUB
             IF   READ-FLG = "1"
                  MOVE  SV-DSPREC     TO  DSP-REC
                  GO             TO   HAC-BACK-SUB
             END-IF
         ELSE
             MOVE   08           TO   ERR-MSG-CD
             MOVE   ZERO         TO   SV-HACNO1
         END-IF
     ELSE
         MOVE   08           TO   ERR-MSG-CD
         MOVE   ZERO         TO   SV-HACNO1
     END-IF.
 HAC-BACK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.4     発注ファイル次レコード読込／表示          *
*----------------------------------------------------------*
 HAC-NEXT-SUB             SECTION.
     MOVE    SPACE        TO   READ-FLG.
     PERFORM    HAC-NEXTSTART-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAH-NEXTREAD-SUB
         IF  EOF-FLG  =  ZERO
             MOVE   DSP-REC    TO   SV-DSPREC
             PERFORM    HEADDEL-SUB
             PERFORM    BODYDEL-SUB
             MOVE   HAH-F02    TO  HACNO1    SV-HACNO1
             PERFORM    HAM-START-SUB
             PERFORM    HAC-SET-SUB
             IF   READ-FLG = "1"
                  MOVE  SV-DSPREC     TO  DSP-REC
                  GO             TO   HAC-NEXT-SUB
             END-IF
         ELSE
             MOVE   09           TO   ERR-MSG-CD
             MOVE   9999999      TO   SV-HACNO1
         END-IF
     ELSE
         MOVE   09           TO   ERR-MSG-CD
         MOVE   9999999      TO   SV-HACNO1
     END-IF.
 HAC-NEXT-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2     発注ファイル読込／表示                    *
*----------------------------------------------------------*
 HACDSP-SUB               SECTION.
     PERFORM    HAH-READ-SUB.
     PERFORM    HAM-START-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAM-READ-SUB
         IF  EOF-FLG  =  ZERO
             PERFORM    HAC-SET-SUB
         ELSE
             MOVE   07           TO   ERR-MSG-CD
         END-IF
     END-IF.
 HACDSP-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2.1   発注データ画面セット                      *
*----------------------------------------------------------*
 HAC-SET-SUB              SECTION.
     PERFORM         EDIT-SET-SYORI.
     PERFORM         EDIT-SET-HEAD.
     PERFORM         EDIT-SET-BODY.
*
     MOVE   SPACE        TO   SAV-AREA.
     INITIALIZE               SAV-AREA.
*
     MOVE     01             TO   JYO-F01.
     MOVE     HAH-F01        TO   JYO-F02.
     MOVE     HAH-F01        TO   DENKU.
     PERFORM  JYO-READ-SUB.
     IF       INVALID-FLG    =    ZERO
              MOVE     JYO-F03    TO   DENKUM
              REWRITE  JYO-REC
     END-IF.
     MOVE     HAH-F10        TO   HACYMD.
     MOVE     HAH-F11        TO   NOUKI.
     MOVE     HAH-F05        TO   TANTO.
     MOVE     HAH-F17        TO   BASYO.
     PERFORM  BSY-READ-SUB.
     MOVE     HAH-F09        TO   RYOHAN.
     MOVE     HAH-F141       TO   SORYOK.
     MOVE     HAH-F142       TO   SORYO.
     EVALUATE   SORYOK
         WHEN   8
                MOVE     NC"自社負担"   TO   SORNM
         WHEN   9
                MOVE     NC"立替"       TO   SORNM
         WHEN   OTHER
                MOVE     SPACE          TO   SORNM
     END-EVALUATE.
     MOVE     HAH-F06        TO   SIRCD.
     PERFORM  SHI-READ-SUB.
     MOVE     HAH-F20        TO   SEIKYU.
     MOVE     HAH-F13        TO   ZEIKU.
     MOVE     HAH-F15        TO   MEMO.
     MOVE     HAH-F07        TO   TOKCD.
     PERFORM  TOK-READ-SUB.
     MOVE     HAH-F16        TO   NONYUC.
     PERFORM  TEN-READ-SUB.
*
     MOVE     HAH-F34        TO   SOFUKB.
     MOVE     HAH-F26        TO   YUBIN.
     MOVE     HAH-F27        TO   OKURI1.
     MOVE     HAH-F28        TO   OKURI2.
     MOVE     HAH-F29        TO   OKURI3.
     MOVE     HAH-F30        TO   TEL.
*
     MOVE     HAH-F321       TO   TEKICD.
     MOVE     HAH-F322       TO   TEKI1.
     MOVE     HAH-F323       TO   TEKI2.
     MOVE     HAH-F311       TO   MSGCD.
     MOVE     HAH-F312       TO   MSG.
     MOVE     HAH-F35(3:4)   TO   SSIME.
*
     MOVE     HAH-F07        TO   SAV-TORICD.
     MOVE     HAH-F17        TO   SAV-SOKO.
     MOVE     HAH-F11        TO   SAV-NOUKI.
*
     PERFORM  UNTIL (EOF-FLG      =  "1"     )  OR
                    (HAM-F02  NOT = SV-HACNO1)
         IF    (HAM-F03      >=   1)  AND
               (HAM-F03      <=   6)
              MOVE   HAM-F03       TO   IXA
              MOVE   HAM-F06       TO   SHOCD (IXA)
              MOVE   HAM-F06       TO   SAV-SHOCD (IXA)
              MOVE   HAM-F07(1:5)  TO   HINTN1(IXA)
              MOVE   HAM-F07(1:5)  TO   SAV-HINTN1(IXA)
              MOVE   HAM-F07(6:2)  TO   HINTN2(IXA)
              MOVE   HAM-F07(6:2)  TO   SAV-HINTN2(IXA)
              MOVE   HAM-F07(8:1)  TO   HINTN3(IXA)
              MOVE   HAM-F07(8:1)  TO   SAV-HINTN3(IXA)
              MOVE   HAM-F09       TO   SURYO (IXA)
              MOVE   HAM-F09       TO   SAV-SURYO (IXA)
              MOVE   HAM-F11       TO   SIRTKB(IXA)
              MOVE   HAM-F12       TO   SIRTAN(IXA)
              MOVE   HAM-F13       TO   GENTKB(IXA)
              MOVE   HAM-F14       TO   GENTAN(IXA)
              MOVE   HAM-F15       TO   HANTAN(IXA)
              MOVE   HAM-F16       TO   BIKOU (IXA)
              PERFORM    MEI-READ-SUB
*
              MOVE   HAM-F08       TO   SAV-TANA  (IXA)
*
         END-IF
         PERFORM    HAM-READ-SUB
     END-PERFORM.
     PERFORM    GOK-COMP-SUB.
 HAC-SET-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.2.1   発注データ画面セット（合計部              *
*----------------------------------------------------------*
 GOK-COMP-SUB             SECTION.
     MOVE    ZERO             TO   SIRGOK.
     MOVE    ZERO             TO   GENGOK.
     PERFORM  VARYING IXA     FROM  1  BY  1
              UNTIL   IXA     >     6
         IF  (SHOCD(IXA)  =  SPACE    )  AND
             (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF  SIRTKB(IXA) = SPACE OR "1"
                 COMPUTE WK-KIN  =  SURYO(IXA) * SIRTAN(IXA)
                 ADD     WK-KIN       TO  SIRGOK
             ELSE
                 ADD     SIRTAN(IXA)  TO  SIRGOK
             END-IF
             IF  GENTKB(IXA) = SPACE OR "1"
                 COMPUTE WK-KIN  =  SURYO(IXA) * GENTAN(IXA)
                 ADD     WK-KIN       TO  GENGOK
             ELSE
                 ADD     GENTAN(IXA)  TO  GENGOK
             END-IF
         END-IF
     END-PERFORM.
 GOK-COMP-END.
     EXIT.
*==========================================================*
*      2.3       ヘッド部入力              MAIN-FLG=3      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG03         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     IF   WK-SYORI  =    1
         MOVE    "HEAD1"     TO   DSP-GROUP
     ELSE
         MOVE    "HEAD2"     TO   DSP-GROUP
     END-IF.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                        IF   WK-SYORI  =    1
                             MOVE   "1"     TO   MAIN-FLG
                        ELSE
                             MOVE   "2"     TO   MAIN-FLG
                        END-IF
         WHEN   "E000"
                        PERFORM   HEADCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "4"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      2.3.1     ヘッド部の入力チェック                    *
*----------------------------------------------------------*
 HEADCHK-SUB             SECTION.
     PERFORM         EDIT-SET-HEAD.
     MOVE       ZERO    TO   ERR-MSG-CD.
****  伝区のチェック  ****
     IF   WK-SYORI  =    1
         IF  DENKU     NOT  NUMERIC
             MOVE   ZERO         TO   DENKU
         END-IF
         MOVE     SPACE          TO   DENKUM
         MOVE     01             TO   JYO-F01
         MOVE     DENKU          TO   JYO-F02
         PERFORM         JYO-READ-SUB
         IF  INVALID-FLG   =   ZERO
             MOVE     JYO-F03    TO   DENKUM
             REWRITE                  JYO-REC
         ELSE
             MOVE   36      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DENKU
             MOVE   "C"     TO   EDIT-CURSOR  OF  DENKU
         END-IF
         IF  ERR-MSG-CD   =    ZERO
             IF   DENKU   =  50  OR  51  OR  60  OR  61
                  CONTINUE
             ELSE
                  IF     ERR-MSG-CD   =    ZERO
                         MOVE    10   TO   ERR-MSG-CD
                  END-IF
                  MOVE   "R"     TO   EDIT-OPTION  OF  DENKU
                  MOVE   "C"     TO   EDIT-CURSOR  OF  DENKU
             END-IF
         END-IF
****     計上フラグのチェック  ****
         IF   KEIJYO    NOT NUMERIC
              MOVE      ZERO      TO   KEIJYO
         END-IF
         IF   KEIJYO    NOT =     ZERO AND  1
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      37   TO   ERR-MSG-CD
              END-IF
              MOVE     "R"   TO   EDIT-OPTION  OF  KEIJYO
              MOVE     "C"   TO   EDIT-CURSOR  OF  KEIJYO
         ELSE
              IF   KEIJYO    =    1
                   IF   DENKU     NOT =     50
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE      38   TO   ERR-MSG-CD
                        END-IF
                        MOVE  "R" TO   EDIT-OPTION  OF  KEIJYO
                        MOVE  "C" TO   EDIT-CURSOR  OF  KEIJYO
                   END-IF
              END-IF
         END-IF
     END-IF.
     IF  KEIJYO    NOT NUMERIC
         MOVE      ZERO      TO   KEIJYO
     END-IF.
****  発注日のチェック  ****
     IF (HACYMD    NOT  NUMERIC)  OR
        (HACYMD    =    ZERO   )
         IF     ERR-MSG-CD   =    ZERO
                MOVE    11   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  HACYMD
         MOVE   "C"     TO   EDIT-CURSOR  OF  HACYMD
     ELSE
         MOVE   HACYMD      TO   CHK-YMD
         PERFORM    YMDCHK-SUB
         IF  INVALID-FLG   =   "1"
             IF     ERR-MSG-CD   =     ZERO
                    MOVE    12   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  HACYMD
             MOVE   "C"     TO   EDIT-CURSOR  OF  HACYMD
         ELSE
             MOVE   LINK-OUT-YMD  TO NEW-HACYMD
         END-IF
     END-IF.
****  納期のチェック  ****
     IF (NOUKI     NOT  NUMERIC)  OR
        (NOUKI     =    ZERO   )
         IF  EDIT-OPTION  OF  HACYMD  NOT =  "R"
             MOVE   HACYMD      TO   CHK-YMD
             PERFORM    NOUKI-SUB
*
             MOVE   LINK-OUT-YMD  TO NEW-NOUKI
         END-IF
     ELSE
         MOVE   NOUKI       TO   CHK-YMD
         PERFORM    YMDCHK-SUB
         IF  INVALID-FLG   =   "1"
             IF     ERR-MSG-CD   =     ZERO
                    MOVE   12      TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  NOUKI
             MOVE   "C"     TO   EDIT-CURSOR  OF  NOUKI
         ELSE
             MOVE   LINK-OUT-YMD  TO NEW-NOUKI
         END-IF
     END-IF.
*
     IF (INVALID-FLG   =   ZERO)  AND
        (NEW-HACYMD    >   NEW-NOUKI)
         MOVE     "1"      TO   INVALID-FLG
         IF       ERR-MSG-CD    =     ZERO
                  MOVE     13   TO    ERR-MSG-CD
         END-IF
         MOVE     "R"      TO   EDIT-OPTION  OF  NOUKI
         MOVE     "C"      TO   EDIT-CURSOR  OF  NOUKI
     END-IF.
*
     MOVE    NEW-NOUKI     TO        WK-NOUHIN.
****  受付担当のチェック  ****
     IF  TANTO     =    SPACE
         IF     ERR-MSG-CD   =    ZERO
                MOVE    14   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  TANTO
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANTO
     END-IF.
****  場所のチェック  ****
     IF  BASYO     =    LOW-VALUE
         MOVE    SPACE          TO  BASYO
     END-IF.
     MOVE    SPACE              TO  BASYOM.
     EVALUATE    TRUE
         WHEN    DENKU  =  50  OR  51
             IF  BASYO     =    SPACE
                 IF     ERR-MSG-CD   =    ZERO
                        MOVE    15   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                 MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
             ELSE
                 PERFORM    BSY-READ-SUB
                 IF  INVALID-FLG   =   "1"
                     IF     ERR-MSG-CD   =    ZERO
                            MOVE   16    TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                     MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
                 END-IF
             END-IF
         WHEN    DENKU  =  60  OR  61
             MOVE   "79"        TO   BASYO
             PERFORM    BSY-READ-SUB
         WHEN    OTHER
             IF  BASYO     =    SPACE
                 CONTINUE
             ELSE
                 PERFORM    BSY-READ-SUB
                 IF  INVALID-FLG   =   "1"
                     IF     ERR-MSG-CD   =    ZERO
                            MOVE    16   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"     TO   EDIT-OPTION  OF  BASYO
                     MOVE   "C"     TO   EDIT-CURSOR  OF  BASYO
                 END-IF
             END-IF
     END-EVALUATE.
****  送料区分のチェック  ****
     IF  SORYOK  NOT NUMERIC
         MOVE    ZERO        TO  SORYOK
     END-IF.
     IF  SORYOK  =  0  OR  8  OR  9
         EVALUATE   SORYOK
             WHEN   8
                    MOVE     NC"自社負担"   TO   SORNM
             WHEN   9
                    MOVE     NC"立替"       TO   SORNM
             WHEN   OTHER
                    MOVE     SPACE          TO   SORNM
         END-EVALUATE
     ELSE
         IF     ERR-MSG-CD   =    ZERO
                MOVE    17   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  SORYOK
         MOVE   "C"     TO   EDIT-CURSOR  OF  SORYOK
     END-IF.
     IF  SORYO   NOT NUMERIC
         MOVE    ZERO        TO  SORYO
     END-IF.
     IF  SORYOK  =   8  OR  9
         IF   SORYO   =   ZERO
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      18   TO   ERR-MSG-CD
              END-IF
              MOVE    "R"    TO   EDIT-OPTION  OF  SORYO
              MOVE    "C"    TO   EDIT-CURSOR  OF  SORYO
         END-IF
     ELSE
         IF   SORYO    >     ZERO
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      19   TO   ERR-MSG-CD
              END-IF
              MOVE    "R"    TO   EDIT-OPTION  OF  SORYO
              MOVE    "C"    TO   EDIT-CURSOR  OF  SORYO
         END-IF
     END-IF.
****  仕入先のチェック  ****
     MOVE   SPACE           TO   SIRNM.
     IF (SIRCD     =    LOW-VALUE) OR
        (SIRCD     =    SPACE    )
         MOVE   SPACE       TO   SIRCD
     ELSE
         PERFORM    SHI-READ-SUB
         IF  INVALID-FLG   =   "1"
             IF     ERR-MSG-CD   =    ZERO
                    MOVE    20   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
         ELSE
*************IF  SIRCD(1:2)  NOT =  WK-BUMON(1:2)
             IF  SIRCD(1:3)  NOT =  292
             AND SIRCD(1:3)  NOT =  291
             AND SIRCD(1:3)  NOT =  293
                 IF     ERR-MSG-CD   =    ZERO
                        MOVE    45   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
             ELSE
*****************2005/04/22 北海道仕入先のチェック追加
              EVALUATE  BASYO
                  WHEN  "T9"  WHEN  "E5"
                  IF  SIRCD(1:3) NOT = 291
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  "T5"  WHEN  "49"
                  IF  SIRCD(1:3) NOT = 293
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  "6A"
                  IF  SIRCD(1:3) NOT = 293
                  AND SIRCD(1:3) NOT = 292
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  OTHER
                  IF  SIRCD(1:3) NOT = 292
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
              END-EVALUATE
             END-IF
         END-IF
     END-IF
****  請求のチェック  ****
     IF (SEIKYU  NUMERIC    )  AND
        (SEIKYU  =  0  OR  9)
         CONTINUE
     ELSE
         IF     ERR-MSG-CD   =    ZERO
                MOVE    21   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  SEIKYU
         MOVE   "C"     TO   EDIT-CURSOR  OF  SEIKYU
     END-IF.
****  税区のチェック  ****
     IF (ZEIKU   NUMERIC    )  AND
        (ZEIKU   =  0  OR  2  OR  8  OR  9)
         CONTINUE
     ELSE
         IF     ERR-MSG-CD   =    ZERO
                MOVE    22   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  ZEIKU
         MOVE   "C"     TO   EDIT-CURSOR  OF  ZEIKU
     END-IF.
****  メモ  ****
     IF  MEMO      =    LOW-VALUE
         MOVE   SPACE       TO   MEMO
     END-IF.
****  得意先／量販■のチェック  ****
     IF  TOKCD   NOT NUMERIC
         MOVE    ZERO        TO  TOKCD
     END-IF.
     IF  RYOHAN  NOT NUMERIC
         MOVE    ZERO        TO  RYOHAN
     END-IF.
     IF  TOKCD   =  ZERO
         IF (DENKU = 60 OR 61 ) OR
            (SIRCD = "29099999"  )
             IF     ERR-MSG-CD   =    ZERO
                    MOVE    23   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
         END-IF
         MOVE    SPACE       TO  TOKNM
     ELSE
         PERFORM    TOK-READ-SUB
         IF  INVALID-FLG   =   "1"
             IF     ERR-MSG-CD   =    ZERO
                    MOVE    24   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
             MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
         ELSE
             IF  SIRCD            =  "29099999"
                 IF   TOKCD(1:5)  =  "99999"
                      CONTINUE
                 ELSE
                     IF     ERR-MSG-CD   =    ZERO
                            MOVE    25   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"     TO   EDIT-OPTION  OF  TOKCD
                     MOVE   "C"     TO   EDIT-CURSOR  OF  TOKCD
                 END-IF
             END-IF
*
             IF (RYOHAN  NOT = ZERO)
                 MOVE    TOK-F94       TO  LI-KBN
                 MOVE    TOK-F92       TO  LI-KETA
                 MOVE    TOK-F60       TO  LI-START
                 MOVE    TOK-F61       TO  LI-END
                 MOVE    RYOHAN        TO  LI-DENNO
                 CALL    "OSKTCDCK"    USING   LINK-AREA
                 IF    TOKCD  NOT =  2363
                   IF  LO-ERR   NOT =  ZERO
                     IF     ERR-MSG-CD   =    ZERO
                            MOVE       26   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"        TO   EDIT-OPTION  OF RYOHAN
                     MOVE   "C"        TO   EDIT-CURSOR  OF RYOHAN
                   END-IF
                 END-IF
              END-IF
         END-IF
     END-IF.
****  納入先のチェック  ****
     IF  NONYUC  NOT NUMERIC
         MOVE    ZERO        TO  NONYUC
     END-IF.
     MOVE   SPACE            TO  NONYUM.
     IF  NONYUC  NOT =  ZERO
*
         IF     TOKCD   NUMERIC
             PERFORM    TEN-READ-SUB
             IF  INVALID-FLG   =   "1"
                 IF     ERR-MSG-CD   =    ZERO
                        MOVE    27   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  NONYUC
                 MOVE   "C"     TO   EDIT-CURSOR  OF  NONYUC
             END-IF
         ELSE
             IF     ERR-MSG-CD   =    ZERO
                    MOVE    28   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  NONYUC
             MOVE   "C"     TO   EDIT-CURSOR  OF  NONYUC
         END-IF
     END-IF.
****  送先区分のチェック  ****
     IF  SOFUKB    NOT NUMERIC
         MOVE    ZERO           TO  SOFUKB
     END-IF.
     IF  OKURI1    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI1
     END-IF.
     IF  OKURI2    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI2
     END-IF.
     IF  OKURI3    =    LOW-VALUE
         MOVE    SPACE          TO  OKURI3
     END-IF.
     EVALUATE    SOFUKB
         WHEN    ZERO
             IF  (YUBIN  = SPACE) AND
                 (OKURI1 = SPACE) AND
                 (OKURI2 = SPACE) AND
                 (OKURI3 = SPACE)
                 CONTINUE
             ELSE
                 IF     ERR-MSG-CD   =     ZERO
                        MOVE    39   TO    ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  SOFUKB
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
             END-IF
         WHEN    1
             IF  DENKU  NOT =   60
                 IF     ERR-MSG-CD   =     ZERO
                        MOVE    42   TO    ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  SOFUKB
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
             ELSE
              IF  (YUBIN  = SPACE) AND
                  (OKURI1 = SPACE) AND
                  (OKURI2 = SPACE) AND
                  (OKURI3 = SPACE)
                 IF     ERR-MSG-CD   =     ZERO
                        MOVE    40   TO    ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  YUBIN
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI1
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI2
                 MOVE   "R"     TO   EDIT-OPTION  OF  OKURI3
                 MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
              END-IF
             END-IF
         WHEN    OTHER
             IF     ERR-MSG-CD   =     ZERO
                    MOVE    41   TO    ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  SOFUKB
             MOVE   "C"     TO   EDIT-CURSOR  OF  SOFUKB
     END-EVALUATE.
****  社外ＭＳＧのチェック  ****
     IF  MSGCD     =    LOW-VALUE
         MOVE    SPACE          TO  MSGCD
     END-IF.
     IF  MSG       =    LOW-VALUE
         MOVE    SPACE          TO  MSG
     END-IF.
     IF  MSG  =  SPACE
         IF  MSGCD  NOT = SPACE
             MOVE     98             TO   JYO-F01
             MOVE     MSGCD          TO   JYO-F02
             PERFORM         JYO-READ-SUB
             IF  INVALID-FLG   =   ZERO
                 MOVE     JYO-F03    TO   MSG
                 REWRITE                  JYO-REC
             ELSE
                 IF     ERR-MSG-CD   =    ZERO
                        MOVE    29   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  MSGCD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  MSGCD
             END-IF
         END-IF
     END-IF.
     IF  SSIME   NOT NUMERIC
         MOVE    ZERO           TO   SSIME
     END-IF.
     IF      DENKU      =    50
             IF  KEIJYO      =   ZERO
                 MOVE   ZERO     TO    SSIME
                 GO   TO   HEADCHK-END
             END-IF
     END-IF.
     IF      DENKU      =    60
             MOVE   ZERO     TO    SSIME
             GO     TO       HEADCHK-END
     END-IF.
     IF  SSIME   =   ZERO
*********MOVE    SYS-DATE2(3:4)  TO  SSIME
         MOVE    NOUKI(1:4)      TO  SSIME
     END-IF.
*支払締日チェック（システム日付（年月）－１から＋１までの範囲）
     IF      SSIME  NOT  NUMERIC
             IF   ERR-MSG-CD     =   ZERO
                  MOVE    43     TO  ERR-MSG-CD
             END-IF
             MOVE   "R"   TO   EDIT-OPTION  OF  SSIME
             MOVE   "C"   TO   EDIT-CURSOR  OF  SSIME
     ELSE
             MOVE  SYS-DATE2(3:4) TO  WK-CHK1-DT   WK-CHK2-DT
             ADD   -1             TO  WK-CHK1-DT2
             IF    WK-CHK1-DT2  <= ZERO
                   ADD    -1      TO  WK-CHK1-DT1
                   MOVE   12      TO  WK-CHK1-DT2
             END-IF
             ADD    1             TO  WK-CHK2-DT2
             IF    WK-CHK2-DT2  >  12
                   ADD     1      TO  WK-CHK2-DT1
                   MOVE    1      TO  WK-CHK2-DT2
             END-IF
             IF    WK-CHK1-DT     <=  SSIME
             AND   WK-CHK2-DT     >=  SSIME
                   CONTINUE
             ELSE
                   IF   ERR-MSG-CD   =   ZERO
                        MOVE    44   TO  ERR-MSG-CD
                   END-IF
                   MOVE   "R"   TO   EDIT-OPTION  OF  SSIME
                   MOVE   "C"   TO   EDIT-CURSOR  OF  SSIME
             END-IF
     END-IF.
 HEADCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.3.1      納期省略時自動算出                       *
*----------------------------------------------------------*
 NOUKI-SUB              SECTION.
*## 1999/12/20 NAV 日付判定方法変更サブルーチン使用
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
*## ＋１日算出
     MOVE     "5"                 TO   LINK-IN-KBN.
     MOVE     1                   TO   LINK-IN-YMD6.
     MOVE     LINK-OUT-YMD        TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD     TO      NOUKI.
 NOUKI-END.
     EXIT.
*==========================================================*
*      2.4       ボディ部入力              MAIN-FLG=4      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG04         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F006"
                        MOVE    "3"    TO   MAIN-FLG
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "5"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB             SECTION.
     PERFORM         EDIT-SET-BODY.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM  VARYING IXA     FROM  1  BY  1
              UNTIL   IXA     >     6
       IF   SHOCD (IXA) =  LOW-VALUE
            MOVE   SPACE      TO   SHOCD (IXA)
       END-IF
       IF   HINTN1(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN1(IXA)
       END-IF
       IF   HINTN2(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN2(IXA)
       END-IF
       IF   HINTN3(IXA) =  LOW-VALUE
            MOVE   SPACE      TO   HINTN3(IXA)
       END-IF
*** 品単　右詰処理 ***
       PERFORM VARYING IXB    FROM  1   BY   1
               UNTIL   IXB    >     4
           PERFORM VARYING IXC    FROM  5   BY  -1
                   UNTIL   IXC    <     2
             IF  HINTN1(IXA)(IXC:1)   =  SPACE
                 COMPUTE IXD    =     IXC   -   1
                 MOVE  HINTN1(IXA)(IXD:1)   TO  HINTN1(IXA)(IXC:1)
                 MOVE  SPACE                TO  HINTN1(IXA)(IXD:1)
             END-IF
           END-PERFORM
       END-PERFORM
*** 商品コード右詰処理  ***
       IF      SHOCD(IXA)     NOT =     SPACE
            PERFORM VARYING IXB    FROM  1   BY   1
                    UNTIL   IXB    >     7
                PERFORM VARYING IXC    FROM  8   BY  -1
                        UNTIL   IXC    <     2
                  IF  SHOCD (IXA)(IXC:1)   =  SPACE
                      COMPUTE IXD    =     IXC   -   1
                      MOVE  SHOCD (IXA)(IXD:1)   TO
                                                SHOCD (IXA)(IXC:1)
                      MOVE  SPACE                TO
                                                SHOCD (IXA)(IXD:1)
                  END-IF
              END-PERFORM
            END-PERFORM
**
            PERFORM     VARYING    IXB   FROM    1   BY   1
                        UNTIL      (IXB   >      7 ) OR
                        (SHOCD(IXA)(IXB:1) NOT =  SPACE)
              IF        SHOCD(IXA)(IXB:1)        =   SPACE
                        MOVE       "0"   TO      SHOCD(IXA)(IXB:1)
              END-IF
            END-PERFORM
       END-IF
**
       IF (  SHOCD(IXA)  =  SPACE    )  AND
          ( (SURYO(IXA)  NOT  NUMERIC)  OR  (SURYO(IXA) = ZERO) )
         MOVE    SPACE        TO    BODY1(IXA)
         MOVE    IXA          TO    IXB
         PERFORM EDIT-SET-GYO
       ELSE
****  商品コード／品単のチェック  ****
         INITIALIZE          MEI-REC
         IF   SHOCD(IXA)  =  SPACE
             IF    ERR-MSG-CD    =     ZERO
                   MOVE     30   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD(IXA)
         ELSE
             PERFORM   MEI-READ-SUB
             IF  INVALID-FLG   =   "1"
                 IF      ERR-MSG-CD    =    ZERO
                         MOVE     31   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"  TO   EDIT-OPTION  OF  SHOCD (IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN1(IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN2(IXA)
                 MOVE   "R"  TO   EDIT-OPTION  OF  HINTN3(IXA)
                 MOVE   "C"  TO   EDIT-CURSOR  OF  SHOCD (IXA)
             ELSE
                 MOVE   MEI-F043     TO   HANTAN(IXA)
                 IF     SIRCD  =  SPACE
                     MOVE    MEI-F05      TO   SIRCD
                     PERFORM    SHI-READ-SUB
                 END-IF
             END-IF
         END-IF
****  数量のチェック  ****
         IF  SURYO(IXA)  NOT  NUMERIC
             MOVE   ZERO         TO  SURYO(IXA)
         END-IF
         IF  MEI-F07  NOT =  ZERO
             DIVIDE  SURYO(IXA)  BY  MEI-F07  GIVING    CHK-SURYO1
                                              REMAINDER CHK-SURYO2
             IF  CHK-SURYO2 NOT = ZERO
                 MOVE   "R"     TO   EDIT-OPTION  OF  SURYO(IXA)
             END-IF
         END-IF
****  仕単／金額のチェック  ****
         IF  SIRTAN(IXA) NOT  NUMERIC
             MOVE   ZERO         TO  SIRTAN(IXA)
         END-IF
         IF  SIRTKB(IXA)  =  SPACE
             MOVE   MEI-F041    TO   SIRTAN(IXA)
                 IF  SIRTAN(IXA) =    ZERO
                     IF      ERR-MSG-CD    =    ZERO
                             MOVE     03   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"  TO   EDIT-OPTION  OF  SIRTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  SIRTAN(IXA)
                 END-IF
         ELSE
             IF  SIRTKB(IXA)  =  "5" OR "6" OR "7"
                 IF  SIRTAN(IXA) NOT = ZERO
                     IF      ERR-MSG-CD    =    ZERO
                             MOVE     33   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"  TO   EDIT-OPTION  OF  SIRTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  SIRTAN(IXA)
                 END-IF
             END-IF
         END-IF
****  仕入単価区分のチェック  ****
         IF  SIRTKB(IXA)  =  SPACE OR "1" OR "3" OR "4" OR "5"
                                   OR "6" OR "7"
             CONTINUE
         ELSE
             IF     ERR-MSG-CD   =     ZERO
                    MOVE    32   TO    ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  SIRTKB(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SIRTKB(IXA)
         END-IF
****  原価単価区分のチェック  ****
         IF  GENTKB(IXA)  =  SPACE OR "1" OR "3" OR "4" OR "5"
                                   OR "6" OR "7"
             CONTINUE
         ELSE
             IF     ERR-MSG-CD   =     ZERO
                    MOVE    32   TO    ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  GENTKB(IXA)
             MOVE   "C"     TO   EDIT-CURSOR  OF  GENTKB(IXA)
         END-IF
****  原単／金額のチェック  ****
         IF  GENTAN(IXA) NOT  NUMERIC
             MOVE   ZERO         TO  GENTAN(IXA)
         END-IF
         IF  GENTKB(IXA)  =  SPACE
             MOVE   MEI-F042    TO   GENTAN(IXA)
         ELSE
             IF  GENTKB(IXA)  =  "5" OR "6" OR "7"
                 IF  GENTAN(IXA) NOT = ZERO
                     IF      ERR-MSG-CD    =    ZERO
                             MOVE     33   TO   ERR-MSG-CD
                     END-IF
                     MOVE   "R"  TO   EDIT-OPTION  OF  GENTAN(IXA)
                     MOVE   "C"  TO   EDIT-CURSOR  OF  GENTAN(IXA)
                 END-IF
             END-IF
         END-IF
       END-IF
     END-PERFORM.
****  摘要コード／摘要のチェック  ****
     IF  TEKICD   NOT  NUMERIC
         MOVE   ZERO         TO  TEKICD
     END-IF.
     IF  TEKICD  =  ZERO
         MOVE   SPACE        TO  TEKI1
         MOVE   SPACE        TO  TEKI2
     ELSE
         IF  TEKICD  =  99
            IF  (TEKI1   =  SPACE)  AND
                (TEKI2   =  SPACE)
                 IF      ERR-MSG-CD    =    ZERO
                         MOVE     34   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"  TO   EDIT-OPTION  OF  TEKI1
                 MOVE   "R"  TO   EDIT-OPTION  OF  TEKI2
                 MOVE   "C"  TO   EDIT-CURSOR  OF  TEKI1
            END-IF
         ELSE
             MOVE     80             TO   JYO-F01
             MOVE     TEKICD         TO   JYO-F02
             PERFORM         JYO-READ-SUB
             IF  INVALID-FLG   =   ZERO
                 MOVE     JYO-F14    TO   TEKI1
                 MOVE     JYO-F15    TO   TEKI2
                 REWRITE                  JYO-REC
             ELSE
                 IF      ERR-MSG-CD  =    ZERO
                         MOVE   29   TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  TEKICD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  TEKICD
             END-IF
         END-IF
     END-IF.
*## 2001/04/12 NAV START ##*
*****IF  SIRCD(1:2)  NOT =  WK-BUMON(1:2)
     IF  SIRCD(1:3)  NOT =  292
     AND SIRCD(1:3)  NOT =  291
     AND SIRCD(1:3)  NOT =  293
         IF     ERR-MSG-CD   =    ZERO
                MOVE    45   TO   ERR-MSG-CD
         END-IF
         MOVE  "R"  TO   EDIT-OPTION  OF  SIRCD
         MOVE  "C"  TO   EDIT-CURSOR  OF  SIRCD
     ELSE
              EVALUATE  BASYO
                  WHEN  "T9"  WHEN  "E5"
                  IF  SIRCD(1:3) NOT = 291
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  "T5"  WHEN  "49"
                  IF  SIRCD(1:3) NOT = 293
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  "6A"
                  IF  SIRCD(1:3) NOT = 293
                  AND SIRCD(1:3) NOT = 292
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
                  WHEN  OTHER
                  IF  SIRCD(1:3) NOT = 292
                      IF     ERR-MSG-CD   =    ZERO
                             MOVE    45   TO   ERR-MSG-CD
                      END-IF
                      MOVE   "R"     TO   EDIT-OPTION  OF  SIRCD
                      MOVE   "C"     TO   EDIT-CURSOR  OF  SIRCD
                  END-IF
              END-EVALUATE
     END-IF.
*## 2001/04/12 NAV END   ##*
****  削除行処理  ****
     IF  ERR-MSG-CD  =  ZERO
         IF  WK-SYORI  =  2
             PERFORM  VARYING IXA     FROM  1  BY  1
                      UNTIL   IXA     >     6
                 IF  (SURYO (IXA)     NUMERIC)  AND
                     (SURYO (IXA)     = ZERO )
                     PERFORM VARYING IXB     FROM  IXA  BY  1
                             UNTIL   IXB     >     6
                         COMPUTE IXC      =    IXB  +  1
                         IF    IXC     <=   6
                          MOVE  BODY1    (IXC)  TO  BODY1    (IXB)
                          MOVE  SPACE           TO  BODY1    (IXC)
                         ELSE
                          MOVE  SPACE           TO  BODY1    (IXB)
                         END-IF
                         PERFORM EDIT-SET-GYO
                     END-PERFORM
                 END-IF
             END-PERFORM
         END-IF
     END-IF.
****  全行未入力チェック／合計計算  ****
     IF  ERR-MSG-CD  =  ZERO
         IF  ( SHOCD(1) = SPACE )  AND
             ( SHOCD(2) = SPACE )  AND
             ( SHOCD(3) = SPACE )  AND
             ( SHOCD(4) = SPACE )  AND
             ( SHOCD(5) = SPACE )  AND
             ( SHOCD(6) = SPACE )
             IF      ERR-MSG-CD  =    ZERO
                     MOVE   35   TO   ERR-MSG-CD
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(1)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(2)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(3)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(4)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(5)
             MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD(6)
             MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD(1)
         ELSE
             PERFORM        GOK-COMP-SUB
         END-IF
     END-IF.
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG04         TO   PFMSG.
     MOVE     "Y"            TO   ANS.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
         WHEN   "F006"
                        IF  WK-SYORI  =  3  OR  9
                            MOVE  "2"       TO   MAIN-FLG
                        ELSE
                            MOVE  "4"       TO   MAIN-FLG
                        END-IF
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  ANS.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  ANS.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     EVALUATE  ANS
         WHEN   "Y"
              PERFORM                 FILUPDT-SUB
              PERFORM                 HEADDEL-SUB
              PERFORM                 BODYDEL-SUB
              IF  WK-SYORI  =  1
                  PERFORM             HACNO-SET-SUB
                  PERFORM             HACYMD-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              ELSE
                  MOVE  "2"       TO  MAIN-FLG
              END-IF
         WHEN   "H"
              IF  WK-SYORI  =  1
                  PERFORM             FILUPDT-SUB
                  PERFORM             BODYDEL-SUB
                  PERFORM             HACNO-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              END-IF
              IF  WK-SYORI  =  2  OR  3
                  PERFORM             FILUPDT-SUB
                  PERFORM             HAC-NEXT-SUB
                  MOVE  "2"       TO  MAIN-FLG
              END-IF
              IF  WK-SYORI  =  9
                  MOVE   06   TO   ERR-MSG-CD
                  MOVE   "R"  TO   EDIT-OPTION  OF  ANS
                  MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
              END-IF
         WHEN   "B"
              IF  WK-SYORI  =  1
                  PERFORM             FILUPDT-SUB
                  PERFORM             HEADDEL-SUB
                  PERFORM             HACNO-SET-SUB
                  PERFORM             HACYMD-SET-SUB
                  MOVE  "3"       TO  MAIN-FLG
              ELSE
                  IF  WK-SYORI  =  2  OR  3
                      MOVE   05   TO   ERR-MSG-CD
                  ELSE
                      MOVE   06   TO   ERR-MSG-CD
                  END-IF
                  MOVE   "R"  TO   EDIT-OPTION  OF  ANS
                  MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
              END-IF
         WHEN   OTHER
              MOVE   04   TO   ERR-MSG-CD
              MOVE   "R"  TO   EDIT-OPTION  OF  ANS
              MOVE   "C"  TO   EDIT-CURSOR  OF  ANS
     END-EVALUATE.
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1    ファイル更新                             *
*----------------------------------------------------------*
 FILUPDT-SUB            SECTION.
     EVALUATE  WK-SYORI
*        登録
         WHEN  1
              PERFORM             HACHDT-OUT-SUB
              EVALUATE  DENKU
                  WHEN  50
                        IF   KEIJYO    =    1
                             PERFORM    NYUKDT-OUT-SUB
                        ELSE
                             CONTINUE
                        END-IF
                  WHEN  60
                        CONTINUE
                  WHEN  OTHER
                        PERFORM   NYUKDT-OUT-SUB
              END-EVALUATE
*        修正
         WHEN  2
              PERFORM             HACHDT-UPD-SUB
              IF   DENKU = 50 OR 60
                   CONTINUE
              ELSE
                   PERFORM        NYUKDT-UPD-SUB
              END-IF
*        削除
         WHEN  3
              PERFORM             HACHED-DLT-SUB
              IF   DENKU = 50 OR 60
                   CONTINUE
              ELSE
                   PERFORM        NYUKDT-DLT-SUB
              END-IF
     END-EVALUATE.
*    商品在庫マスタ更新（５０＝仕入、５１＝仕入返品の時）
     IF     ( DENKU     =   50 OR 51 )
              PERFORM   ZAIMS-UPD-SUB
     END-IF.
 FILUPDT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1  発注ファイル登録                         *
*----------------------------------------------------------*
 HACHDT-OUT-SUB         SECTION.
*    発注ヘッダファイル
     MOVE     SPACE          TO   HAH-REC.
     INITIALIZE                   HAH-REC.
     MOVE     DENKU          TO   HAH-F01.
     MOVE     HACNO1         TO   HAH-F02.
     MOVE     ZERO           TO   HAH-F03.
     MOVE     SPACE          TO   HAH-F18.
     MOVE     SPACE          TO   HAH-F19.
     MOVE     ZERO           TO   HAH-F21.
     MOVE     ZERO           TO   HAH-F22.
     MOVE     ZERO           TO   HAH-F23.
     MOVE     ZERO           TO   HAH-F24.
     MOVE     ZERO           TO   HAH-F33.
     PERFORM  HACHEDF-EDIT-SUB.
     MOVE     SYS-DATE2      TO   HAH-F98.
     MOVE     SYS-DATE2      TO   HAH-F99.
     WRITE    HAH-REC.
*
*    発注明細ファイル
     MOVE    SPACE           TO   HAM-REC.
     INITIALIZE                   HAM-REC.
     MOVE    DENKU           TO   HAM-F01.
     MOVE    HACNO1          TO   HAM-F02.
     MOVE    ZERO            TO   HAM-F03.
     MOVE    ZERO            TO   HAM-F04.
     MOVE    ZERO            TO   HAM-F10.
     MOVE    ZERO            TO   HAM-F17.
     MOVE    SYS-DATE2       TO   HAM-F98.
     MOVE    SYS-DATE2       TO   HAM-F99.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             ADD     1               TO   HAM-F03
             PERFORM   HACMEI-EDIT-SUB
             WRITE                        HAM-REC
         END-IF
     END-PERFORM.
 HACHDT-OUT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.2  発注ヘッダファイル編集                 *
*----------------------------------------------------------*
 HACHEDF-EDIT-SUB      SECTION.
     IF      DENKU = 50 OR 60
         IF  KEIJYO     =    1
             MOVE    1       TO   HAH-F04
             MOVE    1       TO   HAH-F97
             MOVE    NEW-NOUKI    TO   HAH-F12
         ELSE
             MOVE    ZERO    TO   HAH-F04
             MOVE    ZERO    TO   HAH-F12
         END-IF
     ELSE
         MOVE    1           TO   HAH-F04
         MOVE    1           TO   HAH-F97
         MOVE    NEW-NOUKI   TO   HAH-F12
     END-IF.
     MOVE    TANTO           TO   HAH-F05.
     MOVE    SIRCD           TO   HAH-F06.
     MOVE    TOKCD           TO   HAH-F07.
     PERFORM    TOK-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         IF   TOK-F52   NOT NUMERIC
              MOVE      ZERO      TO   HAH-F08
         ELSE
              MOVE      TOK-F52   TO   HAH-F08
         END-IF
     ELSE
         MOVE    ZERO        TO   HAH-F08
     END-IF.
     MOVE    RYOHAN          TO   HAH-F09.
     MOVE    NEW-HACYMD      TO   HAH-F10.
     MOVE    NEW-NOUKI       TO   HAH-F11.
     MOVE    ZEIKU           TO   HAH-F13.
     MOVE    SORYOK          TO   HAH-F141.
     MOVE    SORYO           TO   HAH-F142.
     MOVE    MEMO            TO   HAH-F15.
     MOVE    NONYUC          TO   HAH-F16.
     MOVE    BASYO           TO   HAH-F17.
*
     MOVE    SEIKYU          TO   HAH-F20.
     MOVE    1               TO   HAH-F25.
     MOVE    SOFUKB          TO   HAH-F34.
     MOVE    YUBIN           TO   HAH-F26.
     MOVE    OKURI1          TO   HAH-F27.
     MOVE    OKURI2          TO   HAH-F28.
     MOVE    OKURI3          TO   HAH-F29.
     MOVE    TEL             TO   HAH-F30.
*
     MOVE    MSGCD           TO   HAH-F311.
     MOVE    MSG             TO   HAH-F312.
     MOVE    TEKICD          TO   HAH-F321.
     MOVE    TEKI1           TO   HAH-F322.
     MOVE    TEKI2           TO   HAH-F323.
*支払締日
     IF  SSIME     NOT =     ZERO
         MOVE      SSIME     TO        WK-SSIME-YYMM
         MOVE      01        TO        WK-SSIME-DD
         MOVE     "3"        TO        LINK-IN-KBN
         MOVE      WK-SSIME  TO        LINK-IN-YMD6
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD(1:6)  TO   HAH-F35
     ELSE
         MOVE  ZERO               TO   HAH-F35
     END-IF.
*
 HACHEDF-EDIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.1.3  発注明細ファイル編集                   *
*----------------------------------------------------------*
 HACMEI-EDIT-SUB        SECTION.
     IF      DENKU = 50 OR 60
         IF  KEIJYO     =    1
             MOVE    1       TO   HAM-F05
             MOVE    NEW-NOUKI    TO   HAM-F17
         ELSE
             MOVE    ZERO    TO   HAM-F05
         END-IF
     ELSE
             MOVE    1       TO   HAM-F05
             MOVE    NEW-NOUKI    TO   HAM-F17
     END-IF.
     MOVE    SHOCD (IXA)     TO   HAM-F06.
     MOVE    HINTN1(IXA)     TO   HAM-F07(1:5).
     MOVE    HINTN2(IXA)     TO   HAM-F07(6:2).
     MOVE    HINTN3(IXA)     TO   HAM-F07(8:1).
     MOVE    SURYO (IXA)     TO   HAM-F09.
     MOVE    SIRTKB(IXA)     TO   HAM-F11.
     MOVE    SIRTAN(IXA)     TO   HAM-F12.
     MOVE    GENTKB(IXA)     TO   HAM-F13.
     MOVE    GENTAN(IXA)     TO   HAM-F14.
     MOVE    HANTAN(IXA)     TO   HAM-F15.
     MOVE    BIKOU (IXA)     TO   HAM-F16.
     MOVE    NEW-HACYMD      TO   HAM-F19.
     PERFORM    SHO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE    SHO-F08     TO   HAM-F08
     ELSE
         MOVE    SPACE       TO   HAM-F08
     END-IF.
*
 HACMEI-EDIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  発注ファイル修正                         *
*----------------------------------------------------------*
 HACHDT-UPD-SUB         SECTION.
     INITIALIZE         UPDT-FLG-AREA.
*    発注ヘッダファイル
     PERFORM  HAH-READ-SUB.
     IF  INVALID-FLG    =    ZERO
         PERFORM   HACHEDF-EDIT-SUB
         MOVE      SYS-DATE2      TO   HAH-F99
         REWRITE                       HAH-REC
     END-IF.
*    発注明細ファイル
     PERFORM    HAM-START-SUB.
     IF  EOF-FLG  =  ZERO
         PERFORM    HAM-READ-SUB
     END-IF.
     PERFORM  UNTIL (EOF-FLG      =  "1"  )  OR
                    (HAM-F02  NOT = HACNO1)
         MOVE  HAM-F03      TO   IXA
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
               DELETE             HACMEIF
         ELSE
               PERFORM   HACMEI-EDIT-SUB
               MOVE    SYS-DATE2       TO   HAM-F99
***************2000/12/27 発注ﾃﾞｰﾀのﾕﾘｯｸｽ区分を初期化する
               MOVE    SPACE           TO   HAM-F97
               REWRITE                      HAM-REC
               MOVE    "1"             TO   UPDT-FLG(IXA)
         END-IF
         PERFORM    HAM-READ-SUB
     END-PERFORM.
*  明細行の未更新分追加
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF      UPDT-FLG(IXA)   =    SPACE
                 MOVE    SPACE           TO   HAM-REC
                 INITIALIZE                   HAM-REC
                 MOVE    DENKU           TO   HAM-F01
                 MOVE    HACNO1          TO   HAM-F02
                 MOVE    IXA             TO   HAM-F03
                 MOVE    ZERO            TO   HAM-F04
                 PERFORM   HACMEI-EDIT-SUB
                 MOVE    SYS-DATE2       TO   HAM-F98
                 MOVE    SYS-DATE2       TO   HAM-F99
                 WRITE                        HAM-REC
             END-IF
         END-IF
     END-PERFORM.
 HACHDT-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.3  発注ファイル削除                         *
*----------------------------------------------------------*
 HACHED-DLT-SUB         SECTION.
     PERFORM  HAH-READ-SUB.
     IF  INVALID-FLG    =    ZERO
         MOVE      1              TO   HAH-F24
         MOVE      SYS-DATE2      TO   HAH-F99
         REWRITE                       HAH-REC
     END-IF.
 HACHED-DLT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.4  在庫マスタ更新                           *
*----------------------------------------------------------*
 ZAIMS-UPD-SUB          SECTION.
*
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             MOVE      ZERO       TO     WK-SURYO
             EVALUATE  WK-SYORI
                 WHEN  1
                    MOVE    SURYO(IXA)   TO   WK-SURYO
                 WHEN  2
*    修正後の数量を追加する
                    MOVE    SURYO(IXA)   TO   WK-SURYO
                 WHEN  3
                    COMPUTE WK-SURYO   =
                            SAV-SURYO(IXA)  *   -1
             END-EVALUATE
             PERFORM ZAI-READ-UPDT
         END-IF
     END-PERFORM.
*
*    修正前・削除データで在庫を戻す
     IF  WK-SYORI  =   2
         MOVE        1         TO      TORICD-FLG
         MOVE        SAV-NOUKI TO      NEW-NOUKI
*
         PERFORM   VARYING   IXA  FROM      1    BY   1
                   UNTIL     IXA  >    6
            IF ( SAV-SHOCD(IXA)  =    SPACE  )  AND
               ( SAV-SURYO(IXA)  <=   ZERO   )
                 CONTINUE
            ELSE
                 COMPUTE WK-SURYO = SAV-SURYO(IXA) *  -1
                 MOVE    SAV-SOKO           TO  BASYO
                 MOVE    SAV-SHOCD (IXA)    TO  SHOCD (IXA)
                 MOVE    SAV-HINTN1(IXA)    TO  HINTN1(IXA)
                 MOVE    SAV-HINTN2(IXA)    TO  HINTN2(IXA)
                 MOVE    SAV-HINTN3(IXA)    TO  HINTN3(IXA)
                 PERFORM ZAI-READ-UPDT
             END-IF
         END-PERFORM
         MOVE        ZERO      TO      TORICD-FLG
     END-IF.
 ZAIMS-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5.1  在庫マスタ読込／更新                   *
*----------------------------------------------------------*
 ZAI-READ-UPDT          SECTION.
     MOVE    BASYO           TO   ZAI-F01.
     MOVE    SHOCD (IXA)     TO   ZAI-F021.
     MOVE    HINTN1(IXA)     TO   ZAI-F022(1:5).
     MOVE    HINTN2(IXA)     TO   ZAI-F022(6:2).
     MOVE    HINTN3(IXA)     TO   ZAI-F022(8:1).
*
     IF      TORICD-FLG      =     1
             MOVE  SAV-TANA(IXA)   TO  ZAI-F03 WK-TANA
     ELSE
             PERFORM    SHO-READ-SUB
             IF  INVALID-FLG   =   ZERO
                 MOVE   SHO-F08    TO   ZAI-F03    WK-TANA
             ELSE
                 MOVE   SPACE      TO   ZAI-F03    WK-TANA
             END-IF
     END-IF.
*
     READ    ZAMZAIF
       INVALID      KEY
          MOVE    SPACE           TO   ZAI-REC
          INITIALIZE                   ZAI-REC
          MOVE    BASYO           TO   ZAI-F01
          MOVE    SHOCD (IXA)     TO   ZAI-F021
          MOVE    HINTN1(IXA)     TO   ZAI-F022(1:5)
          MOVE    HINTN2(IXA)     TO   ZAI-F022(6:2)
          MOVE    HINTN3(IXA)     TO   ZAI-F022(8:1)
          MOVE    WK-TANA         TO   ZAI-F03
          PERFORM    MEI-READ-SUB
          IF  INVALID-FLG   =   ZERO
              MOVE    MEI-F031    TO   ZAI-F30
          ELSE
              MOVE    SPACE       TO   ZAI-F30
          END-IF
          IF  TOKCD     NOT       NUMERIC
              MOVE   ZERO         TO   ZAI-F29
          ELSE
              MOVE   TOKCD        TO   ZAI-F29
          END-IF
*
          EVALUATE    DENKU
              WHEN    50
                IF    KEIJYO     NOT =   1
*未入庫数・当月発注数
                      ADD   WK-SURYO   TO   ZAI-F26
                      ADD   WK-SURYO   TO   ZAI-F20
                ELSE
*現在庫数
                      ADD   WK-SURYO   TO   ZAI-F04
                      IF    WK-NOUHIN1     >   WK-LSIME
*次月入庫数
                            ADD   WK-SURYO TO     ZAI-F11
                      ELSE
*当月入庫数・当月入出庫数
                            ADD   WK-SURYO TO     ZAI-F06
                            ADD   WK-SURYO TO     ZAI-F07
                      END-IF
                END-IF
              WHEN    51
                MOVE      NEW-NOUKI      TO     WK-NOUHIN
*現在庫
                SUBTRACT  WK-SURYO       FROM   ZAI-F04
                IF    WK-NOUHIN1     >   WK-LSIME
*次月入庫数
                      SUBTRACT  WK-SURYO       FROM   ZAI-F11
                ELSE
*当月入出庫数・当月入庫数
                      SUBTRACT  WK-SURYO       FROM   ZAI-F06
                      SUBTRACT  WK-SURYO       FROM   ZAI-F07
                END-IF
          END-EVALUATE
          MOVE     SYS-DATE2      TO   ZAI-F98
          MOVE     SYS-DATE2      TO   ZAI-F99
          WRITE    ZAI-REC             END-WRITE
       NOT INVALID  KEY
          EVALUATE    DENKU
              WHEN    50
                IF    KEIJYO     NOT =   1
*未入庫数・当月発注数
                      ADD   WK-SURYO   TO   ZAI-F26
                      ADD   WK-SURYO   TO   ZAI-F20
                ELSE
*現在庫数
                      ADD   WK-SURYO   TO   ZAI-F04
                      IF    WK-NOUHIN1     >   WK-LSIME
*次月入庫数
                            ADD   WK-SURYO TO     ZAI-F11
                      ELSE
*当月入庫数・当月入出庫数
                            ADD   WK-SURYO TO     ZAI-F06
                            ADD   WK-SURYO TO     ZAI-F07
                      END-IF
                END-IF
              WHEN    51
                MOVE      NEW-NOUKI  TO             WK-NOUHIN
*現在庫
                SUBTRACT  WK-SURYO       FROM   ZAI-F04
                IF    WK-NOUHIN1     >   WK-LSIME
*次月入庫数
                      SUBTRACT  WK-SURYO       FROM   ZAI-F11
                ELSE
*当月入出庫数・当月入庫数
                      SUBTRACT  WK-SURYO       FROM   ZAI-F06
                      SUBTRACT  WK-SURYO       FROM   ZAI-F07
                END-IF
          END-EVALUATE
          MOVE     SYS-DATE2      TO   ZAI-F99
          REWRITE  ZAI-REC
     END-READ.
 ZAI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5  入庫ファイル登録                         *
*----------------------------------------------------------*
 NYUKDT-OUT-SUB         SECTION.
     MOVE    SPACE           TO   NYK-REC.
     INITIALIZE                   NYK-REC.
     MOVE    DENKU           TO   NYK-F01.
     MOVE    HACNO1          TO   NYK-F02.
     MOVE    ZERO            TO   NYK-F03.
     MOVE    ZERO            TO   NYK-F04.
     MOVE    ZERO            TO   NYK-F05.
     MOVE    ZERO            TO   NYK-F06.
     MOVE    SYS-DATE2       TO   NYK-F98.
     MOVE    SYS-DATE2       TO   NYK-F99.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             ADD     1               TO   NYK-F05
             PERFORM   NYUKDT-EDIT-SUB
             WRITE                        NYK-REC
         END-IF
     END-PERFORM.
 NYUKDT-OUT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.5.1  入庫ファイル編集                       *
*----------------------------------------------------------*
 NYUKDT-EDIT-SUB        SECTION.
     MOVE    1               TO   NYK-F06.
     MOVE    RYOHAN          TO   NYK-F07.
     MOVE    SIRCD           TO   NYK-F08.
     MOVE    TOKCD           TO   NYK-F09.
     MOVE    ZEIKU           TO   NYK-F10.
     MOVE    SORYOK          TO   NYK-F111.
     MOVE    SORYO           TO   NYK-F112.
     MOVE    NONYUC          TO   NYK-F12.
     MOVE    SPACE           TO   NYK-F32.
     MOVE    NEW-HACYMD      TO   NYK-F13.
     MOVE    NEW-NOUKI       TO   NYK-F14.
     MOVE    SHOCD (IXA)     TO   NYK-F15.
     MOVE    HINTN1(IXA)     TO   NYK-F16(1:5).
     MOVE    HINTN2(IXA)     TO   NYK-F16(6:2).
     MOVE    HINTN3(IXA)     TO   NYK-F16(8:1).
     PERFORM    SHO-READ-SUB.
     IF  INVALID-FLG   =   ZERO
         MOVE    SHO-F08     TO   NYK-F17
     ELSE
         MOVE    SPACE       TO   NYK-F17
     END-IF.
     MOVE    SURYO (IXA)     TO   NYK-F18.
     MOVE    SIRTKB(IXA)     TO   NYK-F19.
     MOVE    SIRTAN(IXA)     TO   NYK-F20.
     MOVE    GENTKB(IXA)     TO   NYK-F21.
     MOVE    GENTAN(IXA)     TO   NYK-F22.
     MOVE    HANTAN(IXA)     TO   NYK-F23.
     MOVE    BIKOU (IXA)     TO   NYK-F24.
     MOVE    NEW-NOUKI       TO   NYK-F26.
     MOVE    BASYO           TO   NYK-F27.
     MOVE    SEIKYU          TO   NYK-F28.
     MOVE    ZERO            TO   NYK-F29.
     MOVE    1               TO   NYK-F30.
     MOVE    ZERO            TO   NYK-F31.
     MOVE    TANTO           TO   NYK-F33.
*支払締日
     IF  SSIME     NOT =     ZERO
         MOVE      SSIME     TO        WK-SSIME-YYMM
         MOVE      01        TO        WK-SSIME-DD
         MOVE     "3"        TO        LINK-IN-KBN
         MOVE      WK-SSIME  TO        LINK-IN-YMD6
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
         MOVE  LINK-OUT-YMD(1:6)  TO   NYK-F34
     ELSE
         MOVE  ZERO               TO   NYK-F34
     END-IF.
 NYUKDT-EDIT-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.6  入庫ファイル修正                         *
*----------------------------------------------------------*
 NYUKDT-UPD-SUB         SECTION.
     INITIALIZE         UPDT-FLG-AREA.
     PERFORM    NYK-START-SUB.
     PERFORM    NYK-READ-SUB.
     PERFORM  UNTIL  EOF-FLG  =  "1"
         MOVE  NYK-F05          TO   IXA
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             DELETE             NYKFILF
         ELSE
             PERFORM   NYUKDT-EDIT-SUB
             MOVE      SYS-DATE2     TO   NYK-F99
             REWRITE                      NYK-REC
             MOVE    "1"             TO   UPDT-FLG(IXA)
         END-IF
         PERFORM    NYK-READ-SUB
     END-PERFORM.
*  未更新分追加
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  6
         IF   (SHOCD(IXA)  =    SPACE  )  AND
              (SURYO(IXA)  NOT  NUMERIC)
             CONTINUE
         ELSE
             IF      UPDT-FLG(IXA)   =    SPACE
                 MOVE    SPACE           TO   NYK-REC
                 INITIALIZE                   NYK-REC
                 MOVE    DENKU           TO   NYK-F01
                 MOVE    HACNO1          TO   NYK-F02
                 MOVE    ZERO            TO   NYK-F03
                 MOVE    IXA             TO   NYK-F05
                 PERFORM   NYUKDT-EDIT-SUB
                 MOVE    SYS-DATE2       TO   NYK-F98
                 MOVE    SYS-DATE2       TO   NYK-F99
                 WRITE                        NYK-REC
             END-IF
         END-IF
     END-PERFORM.
 NYUKDT-UPD-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.7  入庫ファイル削除                         *
*----------------------------------------------------------*
 NYUKDT-DLT-SUB         SECTION.
     PERFORM    NYK-START-SUB.
     PERFORM    NYK-READ-SUB.
     PERFORM  UNTIL  EOF-FLG  =  "1"
         MOVE       1           TO   NYK-F97
         MOVE       SYS-DATE2   TO   NYK-F99
         REWRITE    NYK-REC
         PERFORM    NYK-READ-SUB
     END-PERFORM.
 NYUKDT-DLT-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      HJYOKEN   HTOKMS    HTENMS
              ZSHIMS    ZSOKMS    HMEIMS    HSHOTBL
              ZAMZAIF   HACHEDF   HACMEIF   NYKFILF.
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE      SPACE     TO   ERRMSG
         IF   MAIN-FLG =     4 OR 5
              IF   DENKU     =    50   AND
                   WK-SYORI  =    1    AND
                   KEIJYO    =    1
                   MOVE NC"確認で計上されます"
                                 TO    ERRMSG
              END-IF
         END-IF
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)  TO   ERRMSG
         MOVE    ZERO        TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（処理区分）              *
*----------------------------------------------------------*
 EDIT-SET-SYORI         SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SYORIK.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SYORIK.
 EDIT-SET-SYORI-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.2     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  DENKU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DENKU.
     MOVE   "M"     TO   EDIT-OPTION  OF  KEIJYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KEIJYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  HACNO1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACNO1.
     MOVE   "M"     TO   EDIT-OPTION  OF  HACYMD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACYMD.
     MOVE   "M"     TO   EDIT-OPTION  OF  NOUKI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NOUKI.
     MOVE   "M"     TO   EDIT-OPTION  OF  TANTO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANTO.
     MOVE   "M"     TO   EDIT-OPTION  OF  BASYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  BASYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  RYOHAN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  RYOHAN.
     MOVE   "M"     TO   EDIT-OPTION  OF  SORYOK.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SORYOK.
     MOVE   "M"     TO   EDIT-OPTION  OF  SORYO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SORYO.
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  SEIKYU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SEIKYU.
     MOVE   "M"     TO   EDIT-OPTION  OF  ZEIKU.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  ZEIKU.
     MOVE   "M"     TO   EDIT-OPTION  OF  MEMO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MEMO.
     MOVE   "M"     TO   EDIT-OPTION  OF  TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  NONYUC.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  NONYUC.
     MOVE   "M"     TO   EDIT-OPTION  OF  SOFUKB.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOFUKB.
     MOVE   "M"     TO   EDIT-OPTION  OF  YUBIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  YUBIN.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI1.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEL.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEL.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI2.
     MOVE   "M"     TO   EDIT-OPTION  OF  OKURI3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  OKURI3.
     MOVE   "M"     TO   EDIT-OPTION  OF  MSGCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MSGCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  MSG.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MSG.
     MOVE   "M"     TO   EDIT-OPTION  OF  SSIME.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SSIME.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     6
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKICD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKICD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKI1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKI1.
     MOVE   "M"     TO   EDIT-OPTION  OF  TEKI2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TEKI2.
     MOVE   "M"     TO   EDIT-OPTION  OF  ANS.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  ANS.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SHOCD (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN1(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN1(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN2(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN2(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HINTN3(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HINTN3(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SURYO (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SURYO (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRTKB(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRTKB(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIRTAN(IXB)
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIRTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  GENTKB(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  GENTKB(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  GENTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  GENTAN(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HANTAN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HANTAN(IXB).
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
     MOVE    "FHA00101"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     WRITE    FHA00101.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   HEAD0.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   BODY0.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     条件ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 JYO-READ-SUB           SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
          INITIALIZE              JYO-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.1   発注ヘッダファイルＲＥＡＤ                *
*----------------------------------------------------------*
 HAH-READ-SUB           SECTION.
     MOVE    SPACE           TO   HAH-REC.
     INITIALIZE                   HAH-REC.
     MOVE    HACNO1          TO   HAH-F02.
     READ    HACHEDF
         INVALID
             MOVE     "1"    TO   INVALID-FLG
         NOT INVALID
             MOVE      ZERO  TO   INVALID-FLG
     END-READ.
 HAH-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.1   発注ヘッダファイルＮＥＸＴＲＥＡＤ        *
*----------------------------------------------------------*
 HAH-NEXTREAD-SUB       SECTION.
     READ     HACHEDF   NEXT
         AT END
              MOVE     "1"        TO   EOF-FLG
         NOT AT END
              MOVE      ZERO      TO   EOF-FLG
     END-READ.
     IF  EOF-FLG   =    ZERO
         IF   HAH-F01   =    50   OR   60
              IF   HAH-F04   NOT =     ZERO
                   GO   TO   HAH-NEXTREAD-SUB
              END-IF
         ELSE
              IF   HAH-F22   NOT =     ZERO
                   GO   TO   HAH-NEXTREAD-SUB
              END-IF
         END-IF
*        取消フラグ
         IF   HAH-F24   NOT =     ZERO
              GO   TO   HAH-NEXTREAD-SUB
         END-IF
     END-IF.
*
 HAH-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.1   発注ファイルＳＴＡＲＴ                    *
*----------------------------------------------------------*
 HAM-START-SUB          SECTION.
     MOVE    SPACE           TO   HAM-REC.
     INITIALIZE                   HAM-REC.
     MOVE    HACNO1          TO   HAM-F02.
     MOVE    ZERO            TO   HAM-F03.
     START   HACMEIF    KEY  >=   HAM-F02
                                  HAM-F03
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 HAM-START-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.2   発注ファイルＢＡＣＫＳＴＡＲＴ            *
*----------------------------------------------------------*
 HAC-BACKSTART-SUB      SECTION.
     MOVE    SV-HACNO1       TO   HAH-F02.
     START   HACHEDF    KEY  <    HAH-F02
                                  REVERSED
         INVALID      KEY
             MOVE      "1"        TO   EOF-FLG
         NOT INVALID  KEY
             MOVE       ZERO      TO   EOF-FLG
     END-START.
 HAC-BACKSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2.3   発注ファイルＮＥＸＴＳＴＡＲＴ            *
*----------------------------------------------------------*
 HAC-NEXTSTART-SUB      SECTION.
     MOVE    SV-HACNO1       TO   HAH-F02.
     START   HACHEDF    KEY  >    HAH-F02
         INVALID   KEY
             MOVE      "1"        TO   EOF-FLG
         NOT INVALID  KEY
             MOVE       ZERO      TO   EOF-FLG
     END-START.
 HAC-NEXTSTART-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3.1   発注明細ファイルＲＥＡＤ                  *
*----------------------------------------------------------*
 HAM-READ-SUB           SECTION.
     READ    HACMEIF    NEXT
       AT  END
         MOVE     "1"   TO   EOF-FLG
       NOT AT  END
         IF   (WK-SYORI = 2  OR  3 )  AND
              (HAM-F02  = SV-HACNO1)
              IF  (HAM-F03  >=  1)  AND  (HAM-F03  <= 6)
*                 入庫数がゼロ以外
                  IF   HAM-F10  NOT = ZERO
                       MOVE     "1"        TO   READ-FLG
                       GO        TO   HAM-READ-SUB
                  END-IF
                  IF   HAM-F01 = 50 OR 60
*                      完了区分がゼロ以外
                      IF   HAM-F05  NOT = ZERO
                           MOVE     "1"        TO   READ-FLG
                           GO        TO   HAM-READ-SUB
                      END-IF
                  END-IF
              END-IF
          END-IF
          MOVE     ZERO      TO   EOF-FLG
     END-READ.
 HAM-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.4     場所名の取得                              *
*----------------------------------------------------------*
 BSY-READ-SUB           SECTION.
     MOVE    BASYO           TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     SPACE     TO   BASYOM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SOK-F02   TO   BASYOM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 BSY-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.6     仕入先名の取得                            *
*----------------------------------------------------------*
 SHI-READ-SUB           SECTION.
     MOVE    SIRCD           TO   SHI-F01.
     READ    ZSHIMS
       INVALID      KEY
          MOVE     SPACE     TO   SIRNM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SHI-F02   TO   SIRNM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.8     得意先名の取得                            *
*----------------------------------------------------------*
 TOK-READ-SUB           SECTION.
     MOVE    TOKCD           TO   TOK-F01.
     READ    HTOKMS
       INVALID      KEY
          MOVE     SPACE     TO   TOKNM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     TOK-F02   TO   TOKNM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.9     店舗名の取得                              *
*----------------------------------------------------------*
 TEN-READ-SUB           SECTION.
     MOVE    TOKCD           TO   TEN-F52.
     MOVE    NONYUC          TO   TEN-F011.
     READ    HTENMS
       INVALID      KEY
          MOVE     SPACE     TO   NONYUM
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     TEN-F02   TO   NONYUM
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 TEN-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.10    商品名の取得                              *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
     MOVE    SHOCD (IXA)     TO   MEI-F01.
     MOVE    HINTN1(IXA)     TO   MEI-F0121.
     MOVE    HINTN2(IXA)     TO   MEI-F0122.
     MOVE    HINTN3(IXA)     TO   MEI-F0123.
     READ    HMEIMS
       INVALID      KEY
          MOVE     SPACE     TO   HINMEI(IXA)
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     MEI-F02   TO   HINMEI(IXA)
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.10    商品変換テーブル読込                      *
*----------------------------------------------------------*
 SHO-READ-SUB           SECTION.
     MOVE    TOKCD           TO   SHO-F01.
     MOVE    SHOCD (IXA)     TO   SHO-F031.
     MOVE    HINTN1(IXA)     TO   SHO-F0321.
     MOVE    HINTN2(IXA)     TO   SHO-F0322.
     MOVE    HINTN3(IXA)     TO   SHO-F0323.
     READ    HSHOTBL
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.11    入庫ファイルＳＴＡＲＴ                    *
*----------------------------------------------------------*
 NYK-START-SUB          SECTION.
     MOVE    SPACE           TO   NYK-REC.
     INITIALIZE                   NYK-REC.
     MOVE    HACNO1          TO   NYK-F02.
     MOVE    ZERO            TO   NYK-F03.
     MOVE    ZERO            TO   NYK-F04.
     MOVE    ZERO            TO   NYK-F05.
     START   NYKFILF    KEY  >=   NYK-F02
                                  NYK-F03
                                  NYK-F04
                                  NYK-F05
       INVALID      KEY
          MOVE     "1"       TO   EOF-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   EOF-FLG
     END-START.
 NYK-START-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.12    入庫ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 NYK-READ-SUB           SECTION.
     READ    NYKFILF    NEXT
       AT  END
          MOVE     "1"       TO   EOF-FLG
       NOT AT  END
          IF  NYK-F02   =    HACNO1
              MOVE      ZERO      TO   EOF-FLG
          ELSE
              MOVE      "1"       TO   EOF-FLG
          END-IF
     END-READ.
 NYK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.8       日付の論理チェック                        *
*----------------------------------------------------------*
 YMDCHK-SUB             SECTION.
     MOVE     ZERO                TO   INVALID-FLG.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     CHK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET NOT = ZERO
              MOVE    "1"         TO   INVALID-FLG
     ELSE
              MOVE     ZERO       TO   INVALID-FLG
     END-IF.
*
 YMDCHK-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
