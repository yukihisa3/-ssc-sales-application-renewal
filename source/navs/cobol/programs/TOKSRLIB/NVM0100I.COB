****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　基幹　　　　　　　　　　　        *
*    サブシステム　　　　：　マスタ保守　　　　　　　　　　　　*
*    モジュール名　　　　：　新．商品変換ＴＢＬ保守            *
*    作成日／作成者　　　：　2020/03/19 NAV INOUE              *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　商品名称マスタ・サブ商品マスタの　*
*                            メンテナンスを行う。              *
**変更履歴******************************************************
*    更新日／更新者　　　：　2020/12/15 NAV TAKAHASHI          *
*    変更概要　　　　　　：　直送／セット組区分変更　　　　　　*
*    更新日／更新者　　　：　2024/09/18 NAV TAKAHASHI          *
*    変更概要　　　　　　：　張替区分＝２チェック追加　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVM0100I.
*              流用元： SIT0300I
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
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
                        FILE      STATUS    IS   DSP-STATUS.
*---<<  サブ商品変換テーブル  >>---*
     SELECT   SUBTBLF    ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F01
                                                 SUB-F02
                        FILE      STATUS    IS   SUB-STATUS.
*---<<  商品変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  サブ商品名称マスタ  >>---*
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SBM-F011
                                                 SBM-F0121
                                                 SBM-F0122
                                                 SBM-F0123
                        FILE      STATUS    IS   SBM-STATUS.
*---<<  取引先マスタ　>>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*---<<  倉庫マスタ　  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*---<<  新商品変換テーブル履歴ファイル　  >>---*
     SELECT   HSHOTBR   ASSIGN    TO        DA-01-VI-HSHOTBR1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   STR-F01
                                                 STR-F02
                                                 STR-F03
                        STATUS              IS   STR-STATUS.
*#2020/12/15 NAV ST
*---<<  条件ファイル　  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        STATUS              IS   JYO-STATUS.
*#2020/12/15 NAV ED
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FVM01001  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  サブ商品変換ＴＢＬ　>>---*
 FD  SUBTBLF.
     COPY     SUBTBLF    OF        XFDLIB
              JOINING   SUB       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*---<<  商品名称マスタ　  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  サブ商品名称マスタ　  >>---*
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM       PREFIX.
*---<<  取引先マスタ　　  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  倉庫マスタ　　　  >>---*
 FD ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  新商品変換テーブル履歴ファイル　  >>---*
 FD HSHOTBR.
     COPY     HSHOTBR   OF        XFDLIB
              JOINING   STR       PREFIX.
*#2020/12/15 NAV ST
*---<<  条件ファイル　  >>---*
 FD HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*#2020/12/15 NAV ED
*
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SUB-STATUS          PIC  X(02).
     02  TBL-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  SBM-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  STR-STATUS          PIC  X(02).
*#2020/12/15 NAV ST
     02  JYO-STATUS          PIC  X(02).
*#2020/12/15 NAV ED
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  SUBTBLF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  SUBMEIF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HSHOTBR-INV-FLG     PIC  X(03)  VALUE SPACE.
*#2020/12/15 NAV ST
     02  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
*#2020/12/15 NAV ED
     02  FILE-KBN            PIC  X(01)  VALUE SPACE.
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
     02  ERR-KBN             PIC  X(03)  VALUE SPACE.
     02  SUB-ADD-CNT         PIC  9(07)  VALUE ZERO.
     02  SUB-UPD-CNT         PIC  9(07)  VALUE ZERO.
     02  SUB-DEL-CNT         PIC  9(07)  VALUE ZERO.
     02  TBL-ADD-CNT         PIC  9(07)  VALUE ZERO.
     02  TBL-UPD-CNT         PIC  9(07)  VALUE ZERO.
     02  TBL-DEL-CNT         PIC  9(07)  VALUE ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
     03  WK-TIME4                 PIC  9(04)  VALUE  ZERO.
     03  BK-TIME4                 PIC  9(04)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
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
*商品名編集
 01  WK-SYONM.
     03  WK-SYONM11               PIC  N(15)  VALUE  SPACE.
     03  WK-SYONM12               PIC  N(15)  VALUE  SPACE.
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"Ｆ４：取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"Ｆ４：取消　Ｆ６：項目戻り".
     02  PMSG03            PIC N(20) VALUE
                           NC"Ｆ５：終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVM0100I".
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
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"既に登録済です！".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"処理区分を入力して下さい！".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"処理区分を正しく入力して下さい！".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"必須入力です。必ず入力して下さい！".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"取引先マスタに存在しません！".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"必須入力です。必ず入力して下さい！".
     02  MSG-ERR8            PIC  N(28)  VALUE
            NC"商品変換テーブルに存在しません！".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"自社商品コードを入力してください！".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"商品名称マスタ未登録です！".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"出荷場所を入力して下さい！".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"倉庫マスタ未登録です！".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"張替区分エラー！".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"Ｙを指定して下さい。".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"入数は数字を入力して下さい。".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"振分倉庫ＣＤが未登録です。".
     02  MSG-ERR17           PIC  N(28)  VALUE
            NC"直送／セット組区分エラー！".
     02  MSG-ERR18           PIC  N(28)  VALUE
            NC"ＯＲＤセット区分エラー！".
     02  MSG-ERR19           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR20           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR21           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR22           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR23           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR24           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR25           PIC  N(28)  VALUE
            NC"　".
     02  MSG-ERR26           PIC  N(28)  VALUE
            NC"　".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  26   TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(200).
*
 01  SEQ                   PIC  9(02).
*
****  リンク領域  ***
 LINKAGE               SECTION.
 01  PARA-IN-BUMON        PIC  X(04).
 01  PARA-IN-TANCD        PIC  X(02).
 01  PARA-OUT-DATE        PIC  9(08).
 01  PARA-OUT-TIME        PIC  9(04).
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING
                                             PARA-IN-BUMON
                                             PARA-IN-TANCD
                                             PARA-OUT-DATE
                                             PARA-OUT-TIME.
*
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SUBTBLF.
     MOVE     "SUBTBLL1"      TO   ERR-FL-ID.
     MOVE     SUB-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.

**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "SHOTBL1"      TO   ERR-FL-ID.
     MOVE     TBL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "MEIMS1"       TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE     "TOKMS2"       TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZSOKMS.
     MOVE     "ZSOKMS1 "     TO   ERR-FL-ID.
     MOVE     SOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SUBMEIF.
     MOVE     "SUBMEIL1"     TO   ERR-FL-ID.
     MOVE     SBM-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC8                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBR.
     MOVE     "HSHOTBR1"     TO   ERR-FL-ID.
     MOVE     STR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*#2020/12/15 NAV ST
**
 FILEERR-SEC9                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1 "     TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*#2020/12/15 NAV ED
 END     DECLARATIVES.
****************************************************************
 PROGRAM-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROGRAM-END.
     EXIT.
****************************************************************
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                    SECTION.
*    システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
*    画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
*    システム日付取得
     ACCEPT  WK-TIME  FROM TIME.
*画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
     MOVE  WK-TIME(1:4)     TO  BK-TIME4.
*ファイルオープン
     OPEN  I-O    DSPF.
     OPEN  I-O    SUBTBLF  HSHOTBL HSHOTBR.
     OPEN  INPUT  HTOKMS   HMEIMS  ZSOKMS
                  SUBMEIF.
*#2020/12/15 NAV ST
     OPEN  INPUT  HJYOKEN.
*#2020/12/15 NAV ED
     MOVE  "FVM01001"       TO  DSP-FORMAT.
     MOVE  SPACE            TO  DSP-FVM01001.
     MOVE  SPACE            TO  END-FLG.
     MOVE  "1"              TO  MAIN-FLG.
     MOVE  2                TO  WK-SYORI.
     MOVE  SPACE            TO  DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE  MAIN-FLG
       WHEN  "1"  PERFORM  SYORI-SUB
       WHEN  "2"  PERFORM  HEAD-SUB
       WHEN  "3"  PERFORM  BODY-SUB
       WHEN  "4"  PERFORM  KAKUNIN-SUB
       WHEN  "5"  PERFORM  FILPRT-SUB
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分入力                                              *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG03           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "SYORI"          TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**
** 処理区分入力
**
     MOVE  ZERO             TO  ERR-MSG-CD.
     EVALUATE  DSP-FUNC
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

       WHEN  "E000"
         PERFORM  SYORICHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "2"       TO  MAIN-FLG
            PERFORM  HEADDEL-SUB
         END-IF
       WHEN  OTHER
         MOVE 01            TO  ERR-MSG-CD
     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*    画面表示処理                                              *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE  "SCREEN"         TO  DSP-GROUP.
     MOVE  SPACE            TO  DSP-PROC.
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.
     MOVE  "NVM0100I"       TO  DSP-PROGID.
     MOVE  "FVM01001"       TO  DSP-FORMID.
     WRITE  DSP-FVM01001.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*    エラーメッセージセット                                    *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
*    エラー メッセージ セット
     IF ERR-MSG-CD = ZERO
        MOVE  SPACE         TO  DSP-MSG1
     ELSE
        MOVE  ERR-MSG(ERR-MSG-CD)  TO  DSP-MSG1
        MOVE  ZERO                 TO  ERR-MSG-CD
     END-IF.

 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*    画面データの入力処理                                      *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"             TO  DSP-PROC.
     READ  DSPF.

 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分の入力チェック                                    *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*    処理区分 CHK
     IF ( DSP-SYORI  NOT  NUMERIC   )
        MOVE  3             TO  ERR-MSG-CD
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYORI
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-SYORI
     ELSE
        IF ( DSP-SYORI = 1 OR 2 OR 3 )
           IF DSP-SYORI = 1
              MOVE  1       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 2
              MOVE  2       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 3
              MOVE  3       TO  WK-SYORI
           END-IF
        ELSE
           MOVE  4          TO  ERR-MSG-CD
           MOVE  "R"        TO  EDIT-OPTION  OF  DSP-SYORI
           MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-SYORI
        END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部入力                                            *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "GRPKEY"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
** ヘッド部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

*    アテンション判定
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         MOVE  SPACE            TO  DSP-HEAD01  DSP-HEAD02
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F006"
         MOVE  SPACE            TO  DSP-HEAD01  DSP-HEAD02
         PERFORM  HEADDEL-SUB
         MOVE   "1"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD
       WHEN  "E000"
         PERFORM  HEADDEL-SUB
         PERFORM  HEADCHK-SUB
         IF ERR-MSG-CD = ZERO
            IF WK-SYORI = 3
               MOVE  "4"    TO  MAIN-FLG
            ELSE
               MOVE  "3"    TO  MAIN-FLG
            END-IF
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部消去                                            *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYORI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYORI.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AITECD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AITECD.
*
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部の入力チェック                                  *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*取引先コード入力チェック
     IF DSP-TOKCD  NOT NUMERIC
     OR DSP-TOKCD  =   ZERO
        MOVE  05     TO  ERR-MSG-CD
        MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
        MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
        GO           TO  HEADCHK-END
     ELSE
        MOVE  DSP-TOKCD  TO   TOK-F01
        PERFORM HTOKMS-READ-SEC
        IF  HTOKMS-INV-FLG = "INV"
            MOVE  06     TO  ERR-MSG-CD
            MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
            MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
            GO           TO  HEADCHK-END
        END-IF
     END-IF.
*相手商品ＣＤチェック
     IF DSP-AITECD  =  SPACE
        MOVE  07     TO  ERR-MSG-CD
        MOVE  "R"    TO  EDIT-OPTION OF DSP-AITECD
        MOVE  "C"    TO  EDIT-CURSOR OF DSP-AITECD
        GO           TO  HEADCHK-END
     END-IF.
*
     MOVE      DSP-TOKCD     TO      SUB-F01   TBL-F01.
     MOVE      DSP-AITECD    TO      SUB-F02   TBL-F02.
     PERFORM  SUBTBLF-READ-SEC.
     PERFORM  HSHOTBL-READ-SEC.
*
     EVALUATE  WK-SYORI ALSO SUBTBLF-INV-FLG ALSO HSHOTBL-INV-FLG
         WHEN  "1"  ALSO SPACE  ALSO SPACE *>エラー：既に登録済
               MOVE  02     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-AITECD
               GO           TO  HEADCHK-END
         WHEN  "1"  ALSO SPACE  ALSO "INV" *>サブより
               MOVE   "1"   TO  FILE-KBN
         WHEN  "1"  ALSO "INV"  ALSO SPACE *>ＴＢＬより
               MOVE   "2"   TO  FILE-KBN
         WHEN  "1"  ALSO "INV"  ALSO "INV" *>登録：初期値
               MOVE   "3"   TO  FILE-KBN
         WHEN  "2"  ALSO SPACE  ALSO SPACE *>ＴＢＬ／サブより
               MOVE   "4"   TO  FILE-KBN
         WHEN  "2"  ALSO SPACE  ALSO "INV" *>サブより
               MOVE   "1"   TO  FILE-KBN
         WHEN  "2"  ALSO "INV"  ALSO SPACE *>ＴＢＬより
               MOVE   "2"   TO  FILE-KBN
         WHEN  "2"  ALSO "INV"  ALSO "INV" *>修正：未登録
               MOVE  08     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-AITECD
               GO           TO  HEADCHK-END
         WHEN  "3"  ALSO SPACE  ALSO SPACE *>ＴＢＬ／サブより
               MOVE   "4"   TO  FILE-KBN
         WHEN  "3"  ALSO SPACE  ALSO "INV" *>サブより
               MOVE   "1"   TO  FILE-KBN
         WHEN  "3"  ALSO "INV"  ALSO SPACE *>ＴＢＬより
***************MOVE   "2"   TO  FILE-KBN
               MOVE  08     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-AITECD
               GO           TO  HEADCHK-END
         WHEN  "3"  ALSO "INV"  ALSO "INV" *>削除：未登録
               MOVE  08     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TOKCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-AITECD
               GO           TO  HEADCHK-END
     END-EVALUATE.
*判定
     IF  FILE-KBN  =  "1"
         PERFORM SUBTBLF-SET1-SEC
     END-IF.
*
     IF  FILE-KBN  =  "2"
         PERFORM HSHOTBL-SET1-SEC
     END-IF.
*
     IF  FILE-KBN  =  "4"
         PERFORM SUBTBLF-SET2-SEC
     END-IF.
*
 HEADCHK-END.
     EXIT.

*--------------------------------------------------------------*
*    商品変換ＴＢＬよりセット                                  *
*--------------------------------------------------------------*
 HSHOTBL-SET1-SEC       SECTION.
*
*サカタ商品ＣＤ
        MOVE  TBL-F031           TO  DSP-SHOCD  MEI-F011.
*サカタ品単ＣＤ
        MOVE  TBL-F0321          TO  DSP-TAN1  MEI-F0121.
        MOVE  TBL-F0322          TO  DSP-TAN2  MEI-F0122.
        MOVE  TBL-F0323          TO  DSP-TAN3  MEI-F0123.
        PERFORM  HMEIMS-READ-SEC.
        IF  HMEIMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYONM1
              MOVE  ALL NC"＊"   TO  DSP-SYONM2
************  MOVE  SPACE        TO  DSP-SKJANC
        ELSE
              MOVE  MEI-F021     TO  WK-SYONM11
              MOVE  MEI-F022     TO  WK-SYONM12
              MOVE  WK-SYONM11   TO  DSP-SYONM1
              MOVE  WK-SYONM12   TO  DSP-SYONM2
************* MOVE  MEI-F06      TO  DSP-SKJANC
        END-IF.
*_番
        MOVE  TBL-F08            TO  DSP-TANABN.
*出荷場所
        MOVE  TBL-F04            TO  DSP-SYUKAB  SOK-F01.
        PERFORM  ZSOKMS-READ-SEC.
        IF  ZSOKMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYUKAN
        ELSE
              MOVE  SOK-F02      TO  DSP-SYUKAN
        END-IF.
*分類
        MOVE  TBL-F07            TO  DSP-BUNCD.
*単価
        MOVE  TBL-F09            TO  DSP-SIRTAN.
        MOVE  TBL-F05            TO  DSP-GENTAN.
        MOVE  TBL-F06            TO  DSP-BAITAN.
*検品Ｇ
        MOVE  TBL-F11            TO  DSP-KENGRP.
*張替区分
        MOVE  TBL-F10            TO  DSP-LBLHAR.
*出荷地域区分
        MOVE  TBL-F12            TO  DSP-SYUTIK.
*D365商品CD
*D365JANCD
        MOVE  DSP-SHOCD          TO  SBM-F011.
        MOVE  DSP-TAN1           TO  SBM-F0121.
        MOVE  DSP-TAN2           TO  SBM-F0122.
        MOVE  DSP-TAN3           TO  SBM-F0123.
        PERFORM    SUBMEIF-READ-SEC.
        IF    SUBMEIF-INV-FLG  = SPACE
              MOVE SBM-D02       TO  DSP-D3SYOC
              MOVE SBM-D01       TO  DSP-D3JANC
        ELSE
              MOVE ALL " "       TO  DSP-D3SYOC DSP-D3JANC
        END-IF.
*
 HSHOTBL-SET1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    サブ商品変換ＴＢＬよりセット                              *
*--------------------------------------------------------------*
 SUBTBLF-SET1-SEC       SECTION.
*
*サカタ商品ＣＤ
        MOVE  SUB-F031           TO  DSP-SHOCD  MEI-F011.
*サカタ品単ＣＤ
        MOVE  SUB-F0321          TO  DSP-TAN1  MEI-F0121.
        MOVE  SUB-F0322          TO  DSP-TAN2  MEI-F0122.
        MOVE  SUB-F0323          TO  DSP-TAN3  MEI-F0123.
        PERFORM  HMEIMS-READ-SEC.
        IF  HMEIMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYONM1
              MOVE  ALL NC"＊"   TO  DSP-SYONM2
        ELSE
              MOVE  MEI-F021     TO  WK-SYONM11
              MOVE  MEI-F022     TO  WK-SYONM12
              MOVE  WK-SYONM11   TO  DSP-SYONM1
              MOVE  WK-SYONM12   TO  DSP-SYONM2
        END-IF.
*_番
        MOVE  SUB-F08            TO  DSP-TANABN.
*出荷場所
        MOVE  SUB-F04            TO  DSP-SYUKAB  SOK-F01.
        PERFORM  ZSOKMS-READ-SEC.
        IF  ZSOKMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYUKAN
        ELSE
              MOVE  SOK-F02      TO  DSP-SYUKAN
        END-IF.
*分類
        MOVE  SUB-F07            TO  DSP-BUNCD.
*仕入単価
        MOVE  SUB-F09            TO  DSP-SIRTAN.
*原価単価
        MOVE  SUB-F05            TO  DSP-GENTAN.
*売価単価
        MOVE  SUB-F06            TO  DSP-BAITAN.
*張替区分
        MOVE  SUB-F10            TO  DSP-LBLHAR.
*検品Ｇ
        MOVE  SUB-F11            TO  DSP-KENGRP.
*出荷地域区分
        MOVE  SUB-F12            TO  DSP-SYUTIK.
*振分倉庫
        MOVE  SUB-F16            TO  DSP-FURSOK  SOK-F01.
********PERFORM  ZSOKMS-READ-SEC.
********IF  ZSOKMS-INV-FLG = "INV"
***********   MOVE  ALL NC"＊"   TO  DSP-FSOKXX
***********   MOVE  SPACE        TO  DSP-FSOKXX
******* ELSE
***********   MOVE  SOK-F02      TO  DSP-FSOKXX
********END-IF.
*予備ＪＡＮＣＤ
        MOVE  SUB-F13            TO  DSP-YOBIJN.
*予備インストアＣＤ
        MOVE  SUB-F14            TO  DSP-YOBIIS.
*入数
        MOVE  SUB-F15            TO  DSP-YOBIIR.
*直送/セット組区分
        MOVE  SUB-F19            TO  DSP-TYKSET.
*#2020/12/15 NAV ST
        IF    SUB-F19 NOT =  SPACE
              MOVE   27          TO   JYO-F01
              MOVE   SUB-F19     TO   JYO-F02
              PERFORM    HJYOKEN-READ-SEC
              IF  HJYOKEN-INV-FLG =  "INV"
                  MOVE ALL NC"＊" TO  DSP-ORDNM
              ELSE
                  MOVE JYO-F03    TO  DSP-ORDNM
              END-IF
        ELSE
              MOVE     SPACE      TO  DSP-ORDNM
        END-IF.
*#2020/12/15 NAV ED
*
*D365商品CD
*D365JANCD
        MOVE  DSP-SHOCD          TO  SBM-F011.
        MOVE  DSP-TAN1           TO  SBM-F0121.
        MOVE  DSP-TAN2           TO  SBM-F0122.
        MOVE  DSP-TAN3           TO  SBM-F0123.
        PERFORM    SUBMEIF-READ-SEC.
        IF    SUBMEIF-INV-FLG  = SPACE
              MOVE SBM-D02       TO  DSP-D3SYOC
              MOVE SBM-D01       TO  DSP-D3JANC
        ELSE
              MOVE ALL " "       TO  DSP-D3SYOC DSP-D3JANC
        END-IF.
*登録者情報
        MOVE  SUB-F94(1:4)       TO  DSP-ADDDAT(1:4).
        MOVE  "/"                TO  DSP-ADDDAT(5:1).
        MOVE  SUB-F94(5:2)       TO  DSP-ADDDAT(6:2).
        MOVE  "/"                TO  DSP-ADDDAT(8:1).
        MOVE  SUB-F94(7:2)       TO  DSP-ADDDAT(9:2).
********MOVE  SUB-F95(1:2)       TO  DSP-TTIME(1:2).
******* MOVE  ":"                TO  DSP-TTIME(3:1).
******  MOVE  SUB-F95(3:2)       TO  DSP-TTIME(4:2).
******* MOVE  ":"                TO  DSP-TTIME(6:1).
******* MOVE  SUB-F95(5:2)       TO  DSP-TTIME(7:2).
******* MOVE  SUB-F92            TO  DSP-ADDTAN(1:4).
******* MOVE  "-"                TO  DSP-ADDTAN(5:1).
        MOVE  SUB-F93            TO  DSP-ADDTAN.
*更新者情報
        MOVE  SUB-F98(1:4)       TO  DSP-UPDDAT(1:4).
        MOVE  "/"                TO  DSP-UPDDAT(5:1).
        MOVE  SUB-F98(5:2)       TO  DSP-UPDDAT(6:2).
        MOVE  "/"                TO  DSP-UPDDAT(8:1).
        MOVE  SUB-F98(7:2)       TO  DSP-UPDDAT(9:2).
******* MOVE  SUB-F99(1:2)       TO  DSP-KTIME(1:2).
******  MOVE  ":"                TO  DSP-KTIME(3:1).
*****   MOVE  SUB-F99(3:2)       TO  DSP-KTIME(4:2).
*****   MOVE  ":"                TO  DSP-KTIME(6:1).
*****   MOVE  SUB-F99(5:2)       TO  DSP-KTIME(7:2).
******  MOVE  SUB-F96            TO  DSP-UPDTAN(1:4).
******  MOVE  "-"                TO  DSP-UPDTAN(5:1).
        MOVE  SUB-F97            TO  DSP-UPDTAN.
*
 SUBTBLF-SET1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    サブ商品変換ＴＢＬよりセット                              *
*--------------------------------------------------------------*
 SUBTBLF-SET2-SEC       SECTION.
*
*サカタ商品ＣＤ
        MOVE  SUB-F031           TO  DSP-SHOCD  MEI-F011.
*サカタ品単ＣＤ
        MOVE  SUB-F0321          TO  DSP-TAN1  MEI-F0121.
        MOVE  SUB-F0322          TO  DSP-TAN2  MEI-F0122.
        MOVE  SUB-F0323          TO  DSP-TAN3  MEI-F0123.
        PERFORM  HMEIMS-READ-SEC.
        IF  HMEIMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYONM1
              MOVE  ALL NC"＊"   TO  DSP-SYONM2
***********   MOVE  SPACE        TO  DSP-SKJANC
        ELSE
              MOVE  MEI-F021     TO  WK-SYONM11
              MOVE  MEI-F022     TO  WK-SYONM12
              MOVE  WK-SYONM11   TO  DSP-SYONM1
              MOVE  WK-SYONM12   TO  DSP-SYONM2
***********   MOVE  MEI-F06      TO  DSP-SKJANC
        END-IF.
*_番
        MOVE  SUB-F08            TO  DSP-TANABN.
*出荷場所
        MOVE  SUB-F04            TO  DSP-SYUKAB  SOK-F01.
        PERFORM  ZSOKMS-READ-SEC.
        IF  ZSOKMS-INV-FLG = "INV"
              MOVE  ALL NC"＊"   TO  DSP-SYUKAN
        ELSE
              MOVE  SOK-F02      TO  DSP-SYUKAN
        END-IF.
*分類
        MOVE  SUB-F07            TO  DSP-BUNCD.
*仕入単価
        MOVE  SUB-F09            TO  DSP-SIRTAN.
*原価単価
        MOVE  SUB-F05            TO  DSP-GENTAN.
*売価単価
        MOVE  SUB-F06            TO  DSP-BAITAN.
*張替区分
        MOVE  SUB-F10            TO  DSP-LBLHAR.
*検品Ｇ
        MOVE  SUB-F11            TO  DSP-KENGRP.
*出荷地域区分
        MOVE  SUB-F12            TO  DSP-SYUTIK.
*振分倉庫
        MOVE  SUB-F16            TO  DSP-FURSOK  SOK-F01.
********PERFORM  ZSOKMS-READ-SEC.
******* IF  ZSOKMS-INV-FLG = "INV"
**************MOVE  ALL NC"＊"   TO  DSP-FSOKXX
**************MOVE  SPACE        TO  DSP-FSOKXX
******* ELSE
************* MOVE  SOK-F02      TO  DSP-FSOKXX
********END-IF.
*
*予備ＪＡＮＣＤ
        MOVE  SUB-F13            TO  DSP-YOBIJN.
*予備インストアＣＤ
        MOVE  SUB-F14            TO  DSP-YOBIIS.
*
*入数
        MOVE  SUB-F15            TO  DSP-YOBIIR.
*
*直送/セット組区分
        MOVE  SUB-F19            TO  DSP-TYKSET.
*#2020/12/15 NAV ST
        IF    SUB-F19 NOT =  SPACE
              MOVE   27          TO   JYO-F01
              MOVE   SUB-F19     TO   JYO-F02
              PERFORM    HJYOKEN-READ-SEC
              IF  HJYOKEN-INV-FLG =  "INV"
                  MOVE ALL NC"＊" TO  DSP-ORDNM
              ELSE
                  MOVE JYO-F03    TO  DSP-ORDNM
              END-IF
        ELSE
              MOVE     SPACE      TO  DSP-ORDNM
        END-IF.
*#2020/12/15 NAV ED
*
*D365商品CD
*D365JANCD
        MOVE  DSP-SHOCD          TO  SBM-F011.
        MOVE  DSP-TAN1           TO  SBM-F0121.
        MOVE  DSP-TAN2           TO  SBM-F0122.
        MOVE  DSP-TAN3           TO  SBM-F0123.
        PERFORM    SUBMEIF-READ-SEC.
        IF    SUBMEIF-INV-FLG  = SPACE
              MOVE SBM-D02       TO  DSP-D3SYOC
              MOVE SBM-D01       TO  DSP-D3JANC
        ELSE
              MOVE ALL " "       TO  DSP-D3SYOC DSP-D3JANC
        END-IF.
*登録者情報
        MOVE  SUB-F94(1:4)       TO  DSP-ADDDAT(1:4).
        MOVE  "/"                TO  DSP-ADDDAT(5:1).
        MOVE  SUB-F94(5:2)       TO  DSP-ADDDAT(6:2).
        MOVE  "/"                TO  DSP-ADDDAT(8:1).
        MOVE  SUB-F94(7:2)       TO  DSP-ADDDAT(9:2).
********MOVE  SUB-F95(1:2)       TO  DSP-TTIME(1:2).
********MOVE  ":"                TO  DSP-TTIME(3:1).
******* MOVE  SUB-F95(3:2)       TO  DSP-TTIME(4:2).
********MOVE  ":"                TO  DSP-TTIME(6:1).
******* MOVE  SUB-F95(5:2)       TO  DSP-TTIME(7:2).
******* MOVE  SUB-F92            TO  DSP-ADDTAN(1:4).
******* MOVE  "-"                TO  DSP-ADDTAN(5:1).
        MOVE  SUB-F93            TO  DSP-ADDTAN.
*更新者情報
        MOVE  SUB-F98(1:4)       TO  DSP-UPDDAT(1:4).
        MOVE  "/"                TO  DSP-UPDDAT(5:1).
        MOVE  SUB-F98(5:2)       TO  DSP-UPDDAT(6:2).
        MOVE  "/"                TO  DSP-UPDDAT(8:1).
        MOVE  SUB-F98(7:2)       TO  DSP-UPDDAT(9:2).
******  MOVE  SUB-F99(1:2)       TO  DSP-KTIME(1:2).
*****   MOVE  ":"                TO  DSP-KTIME(3:1).
******* MOVE  SUB-F99(3:2)       TO  DSP-KTIME(4:2).
*****   MOVE  ":"                TO  DSP-KTIME(6:1).
******  MOVE  SUB-F99(5:2)       TO  DSP-KTIME(7:2).
******  MOVE  SUB-F96            TO  DSP-UPDTAN(1:4).
******  MOVE  "-"                TO  DSP-UPDTAN(5:1).
        MOVE  SUB-F97            TO  DSP-UPDTAN.
*
 SUBTBLF-SET2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部入力                                            *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "GRP001"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**  ボディー部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

     EVALUATE  DSP-FUNC
       WHEN  "F004"
         MOVE  SPACE            TO  DSP-HEAD01  DSP-HEAD02
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F006"
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE   "2"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD
       WHEN  "E000"
         PERFORM  HEADDEL-SUB
         PERFORM  BODYDEL-SUB
         PERFORM  BODYCHK-SUB
         IF ERR-MSG-CD = ZERO
            PERFORM  BODYDEL-SUB
            MOVE  "4"       TO  MAIN-FLG
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部消去                                            *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
*
*****MOVE  SPACE            TO  DSP-MEISAI.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHOCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANABN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANABN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYUKAB.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYUKAB.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUNCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUNCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SIRTAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SIRTAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENTAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENTAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAITAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAITAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-LBLHAR.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-LBLHAR.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGRP.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGRP.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYUTIK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYUTIK.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-FURSOK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-FURSOK.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-YOBIJN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YOBIJN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-YOBIIS.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YOBIIS.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-YOBIIR.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YOBIIR.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TYKSET.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TYKSET.
*
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     MOVE  ZERO             TO  ERR-MSG-CD.
     PERFORM  BODYDEL-SUB.
*サカタ商品ＣＤ入力チェック
     IF DSP-SHOCD = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SHOCD
        IF ERR-MSG-CD = ZERO
           MOVE  09         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SHOCD
        END-IF
     ELSE
*       商品マスタ存在チェック
           MOVE DSP-SHOCD  TO  MEI-F011
           MOVE DSP-TAN1  TO  MEI-F0121
           MOVE DSP-TAN2  TO  MEI-F0122
           MOVE DSP-TAN3  TO  MEI-F0123
           PERFORM HMEIMS-READ-SEC
           IF  HMEIMS-INV-FLG = "INV"
               MOVE  "R"    TO  EDIT-OPTION OF DSP-SHOCD
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN1
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN2
               MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN3
               IF ERR-MSG-CD = ZERO
                  MOVE  10  TO  ERR-MSG-CD
                  MOVE  "C" TO  EDIT-CURSOR OF DSP-SHOCD
               END-IF
           ELSE
*商品名
               MOVE  MEI-F021   TO   DSP-SYONM1
               MOVE  MEI-F022   TO   DSP-SYONM2
           END-IF
*D365商品CD
*D365JANCD
           IF    DSP-SYORI = 1  OR   2
                 MOVE  DSP-SHOCD          TO  SBM-F011
                 MOVE  DSP-TAN1           TO  SBM-F0121
                 MOVE  DSP-TAN2           TO  SBM-F0122
                 MOVE  DSP-TAN3           TO  SBM-F0123
                 PERFORM    SUBMEIF-READ-SEC
                 IF    SUBMEIF-INV-FLG  = SPACE
                       MOVE SBM-D02     TO  DSP-D3SYOC
                       MOVE SBM-D01     TO  DSP-D3JANC
                 ELSE
                       MOVE ALL " "     TO  DSP-D3SYOC DSP-D3JANC
                 END-IF
           END-IF
     END-IF.
*出荷場所
     IF DSP-SYUKAB = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYUKAB
        IF ERR-MSG-CD = ZERO
           MOVE  11         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SYUKAB
        END-IF
     ELSE
        MOVE  DSP-SYUKAB          TO  SOK-F01
        PERFORM  ZSOKMS-READ-SEC
        IF  ZSOKMS-INV-FLG = "INV"
            MOVE  "R"       TO  EDIT-OPTION OF DSP-SYUKAB
            IF ERR-MSG-CD = ZERO
               MOVE  12     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-SYUKAB
            END-IF
        ELSE
            MOVE  SOK-F02   TO  DSP-SYUKAN
        END-IF
     END-IF.
*張替区分
*#2024/09/18 NAV ST 張替区分＝２追加
*****IF DSP-LBLHAR = SPACE  OR  "1"
     IF DSP-LBLHAR = SPACE  OR  "1"  OR  "2"
*#2024/09/18 NAV ED
        CONTINUE
     ELSE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-LBLHAR
        IF ERR-MSG-CD = ZERO
           MOVE  13         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-LBLHAR
        END-IF
     END-IF.
*振分倉庫
     IF DSP-FURSOK NOT = SPACE
        MOVE  DSP-FURSOK         TO  SOK-F01
        PERFORM  ZSOKMS-READ-SEC
        IF  ZSOKMS-INV-FLG = "INV"
            MOVE  "R"       TO  EDIT-OPTION OF DSP-FURSOK
            IF ERR-MSG-CD = ZERO
               MOVE  12     TO  ERR-MSG-CD
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-FURSOK
            END-IF
********ELSE
**********  MOVE  SOK-F02   TO  DSP-FSOKXX
        END-IF
*****ELSE
*********   MOVE  SPACE     TO  DSP-FSOKXX
     END-IF.
*入数
     IF DSP-YOBIIR NOT NUMERIC
        IF ERR-MSG-CD = ZERO
           MOVE  "R"        TO  EDIT-OPTION OF DSP-YOBIIR
           MOVE  15         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-YOBIIR
        END-IF
     END-IF.
*直送/セット組区分
*#2020/12/15 NAV ST
*TTTTIF DSP-TYKSET = SPACE  OR  "1"  OR  "2"
*TT     CONTINUE
*TT  ELSE
*TT     MOVE  "R"           TO  EDIT-OPTION OF DSP-TYKSET
*TT     IF ERR-MSG-CD = ZERO
*TT        MOVE  17         TO  ERR-MSG-CD
*TT        MOVE  "C"        TO  EDIT-CURSOR OF DSP-TYKSET
*TT     END-IF
*TTTTEND-IF.
**#2020/12/15 NAV ST
     IF DSP-TYKSET = SPACE
        MOVE SPACE          TO  DSP-ORDNM
     ELSE
        MOVE   27           TO  JYO-F01
        MOVE   DSP-TYKSET   TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG  =  "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-TYKSET
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-TYKSET
            END-IF
            MOVE  SPACE         TO  DSP-ORDNM
        ELSE
            MOVE  JYO-F03       TO  DSP-ORDNM
        END-IF
     END-IF.
*#2020/12/15 NAV ED
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*    確認入力                                                  *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF ERR-MSG-CD = ZERO
        MOVE  "Y"           TO  DSP-KAKUNI
     END-IF.

     PERFORM  MSG-SEC.
     IF WK-SYORI = 3
        MOVE  PMSG01        TO  DSP-MSG2
     ELSE
        MOVE  PMSG02        TO  DSP-MSG2
     END-IF.

     PERFORM  DSP-WRITE-SUB.
     MOVE  "KAKUNI"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
     MOVE  ZERO             TO  ERR-MSG-CD.
 KAKUNIN.
** 確認
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         MOVE  SPACE            TO  DSP-HEAD01  DSP-HEAD02
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "F006"
         IF WK-SYORI = 3
            MOVE  01        TO  ERR-MSG-CD
         ELSE
            MOVE  "3"       TO  MAIN-FLG
         END-IF
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "E000"
         IF DSP-KAKUNI NOT = "Y"
            MOVE  14        TO  ERR-MSG-CD
         ELSE
            MOVE  SPACE     TO  DSP-KAKUNI
            MOVE  "5"       TO  MAIN-FLG
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD

     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*    ファイル更新                                              *
*--------------------------------------------------------------*
 FILPRT-SUB             SECTION.
*
 FILPRT000.
*パラメタOUTセット
     MOVE  SYS-DATE         TO  PARA-OUT-DATE.
***  MOVE  WK-TIME          TO  WK-TIME4.
     MOVE  BK-TIME4         TO  PARA-OUT-TIME.
*履歴ファイルセット
* 入力日
     MOVE  SYS-DATE         TO  STR-F01.
* 入力時刻
     MOVE  BK-TIME4         TO  STR-F02.
* 入力担当者
     MOVE  PARA-IN-TANCD    TO  STR-F03.
* モード
     MOVE  DSP-SYORI        TO  STR-F04.
 FILPRT001.
* 変更前レコードエリア
     IF  ( DSP-SYORI = "1" )
           INITIALIZE       STR-A00
     END-IF.
     IF  ( DSP-SYORI = "2"  AND FILE-KBN = "1" ) OR
         ( DSP-SYORI = "2"  AND FILE-KBN = "4" ) OR
         ( DSP-SYORI = "3"  AND FILE-KBN = "1" ) OR
         ( DSP-SYORI = "3"  AND FILE-KBN = "4" )
           PERFORM          SUB-TO-RIREKI-SET-SEC
     END-IF.
     IF  ( DSP-SYORI = "2"  AND FILE-KBN = "2" )
           PERFORM          TBL-TO-RIREKI-SET-SEC
     END-IF.
*
 FILPRT005.
* 削除の場合
     IF DSP-SYORI = "3"
        IF HSHOTBL-INV-FLG = SPACE
           DELETE HSHOTBL
        END-IF
        IF SUBTBLF-INV-FLG = SPACE
           DELETE SUBTBLF
        END-IF
        ADD    1            TO  SUB-DEL-CNT
        GO                  TO  FILPRT888
     END-IF.
*
* 登録・修正の場合
 FILPRT010.
     IF FILE-KBN =  "1"
        MOVE  SPACE         TO  TBL-REC
        INITIALIZE              TBL-REC
     END-IF.
*
 FILPRT020.
     IF FILE-KBN =  "2"
        MOVE  SPACE         TO  SUB-REC
        INITIALIZE              SUB-REC
     END-IF.
*
 FILPRT030.
     IF FILE-KBN =  "3"
        MOVE  SPACE         TO  SUB-REC  TBL-REC
        INITIALIZE              SUB-REC  TBL-REC
     END-IF.
*
*商品変換ＴＢＬセット・更新
*サブ商品変換ＴＢＬセット・更新
*履歴ファイルセット
 FILPRT040.
*
     INITIALIZE                 STR-B00.
     MOVE  DSP-TOKCD        TO  TBL-F01     SUB-F01    STR-B01.
     MOVE  DSP-AITECD       TO  TBL-F02     SUB-F02    STR-B02.
     MOVE  DSP-SHOCD        TO  TBL-F031    SUB-F031   STR-B031.
     MOVE  DSP-TAN1         TO  TBL-F0321   SUB-F0321  STR-B0321.
     MOVE  DSP-TAN2         TO  TBL-F0322   SUB-F0322  STR-B0322.
     MOVE  DSP-TAN3         TO  TBL-F0323   SUB-F0323  STR-B0323.
     MOVE  DSP-SYUKAB       TO  TBL-F04     SUB-F04    STR-B04.
     MOVE  DSP-GENTAN       TO  TBL-F05     SUB-F05    STR-B05.
     MOVE  DSP-BAITAN       TO  TBL-F06     SUB-F06    STR-B06.
     MOVE  DSP-BUNCD        TO  TBL-F07     SUB-F07    STR-B07.
     MOVE  DSP-TANABN       TO  TBL-F08     SUB-F08    STR-B08.
     MOVE  DSP-SIRTAN       TO  TBL-F09     SUB-F09    STR-B09.
     MOVE  DSP-LBLHAR       TO  TBL-F10     SUB-F10    STR-B10.
     MOVE  DSP-KENGRP       TO  TBL-F11     SUB-F11    STR-B11.
     MOVE  DSP-SYUTIK       TO  TBL-F12     SUB-F12    STR-B12.
     MOVE  DSP-YOBIJN       TO              SUB-F13    STR-B13.
     MOVE  DSP-YOBIIS       TO              SUB-F14    STR-B14.
     MOVE  DSP-YOBIIR       TO              SUB-F15    STR-B15.
     MOVE  DSP-FURSOK       TO              SUB-F16    STR-B16.
     MOVE  DSP-D3JANC       TO              SUB-F17    STR-B17.
     MOVE  DSP-D3SYOC       TO              SUB-F18    STR-B18.
     MOVE  DSP-TYKSET       TO              SUB-F19    STR-B19.
*
     EVALUATE  FILE-KBN
         WHEN  "1"
           MOVE PARA-IN-TANCD    TO  TBL-F13
           MOVE SYS-DATE         TO  TBL-F98
           MOVE PARA-IN-BUMON    TO  SUB-F96
           MOVE PARA-IN-TANCD    TO  SUB-F97
           MOVE SYS-DATE         TO  SUB-F98
           ACCEPT  WK-TIME  FROM TIME
           MOVE WK-TIME(1:6)     TO  SUB-F99
           REWRITE  SUB-REC
           WRITE    TBL-REC
           ADD  1                TO  SUB-UPD-CNT
           ADD  1             TO  TBL-ADD-CNT
         WHEN  "2"
           MOVE PARA-IN-TANCD    TO  TBL-F14
           MOVE SYS-DATE      TO  TBL-F99
           MOVE PARA-IN-BUMON    TO  SUB-F92  SUB-F96
           MOVE PARA-IN-TANCD    TO  SUB-F93  SUB-F97
           MOVE SYS-DATE      TO  SUB-F94  SUB-F98
           ACCEPT  WK-TIME  FROM TIME
           MOVE WK-TIME(1:6)  TO  SUB-F95  SUB-F99
           WRITE    SUB-REC
           REWRITE  TBL-REC
           ADD  1             TO  SUB-ADD-CNT
           ADD  1             TO  TBL-UPD-CNT
         WHEN  "3"
           MOVE PARA-IN-TANCD    TO  TBL-F13  TBL-F14
           MOVE SYS-DATE      TO  TBL-F98  TBL-F99
           MOVE PARA-IN-BUMON    TO  SUB-F92  SUB-F96
           MOVE PARA-IN-TANCD    TO  SUB-F93  SUB-F97
           MOVE SYS-DATE      TO  SUB-F94  SUB-F98
           ACCEPT  WK-TIME  FROM TIME
           MOVE WK-TIME(1:6)  TO  SUB-F95  SUB-F99
           WRITE    SUB-REC
           WRITE    TBL-REC
           ADD  1             TO  SUB-ADD-CNT
           ADD  1             TO  TBL-ADD-CNT
         WHEN  "4"
           MOVE PARA-IN-TANCD    TO  TBL-F14
           MOVE SYS-DATE      TO  TBL-F99
           MOVE PARA-IN-BUMON TO  SUB-F96
           MOVE PARA-IN-TANCD    TO  SUB-F97
           MOVE SYS-DATE      TO  SUB-F98
           ACCEPT  WK-TIME  FROM TIME
           MOVE WK-TIME(1:6)  TO  SUB-F99
           REWRITE  SUB-REC
           REWRITE  TBL-REC
           ADD  1             TO  SUB-UPD-CNT
           ADD  1             TO  TBL-UPD-CNT
     END-EVALUATE.
*
 FILPRT888.
*履歴ファイル共通部セット
     IF    DSP-SYORI  =  1
           MOVE  PARA-IN-BUMON    TO        STR-B92
           MOVE  PARA-IN-TANCD    TO        STR-B93
           MOVE  SYS-DATE         TO        STR-B94
           MOVE  WK-TIME(1:6)     TO        STR-B95
     END-IF.
     IF    DSP-SYORI  =  2
           MOVE  PARA-IN-BUMON    TO        STR-B96
           MOVE  PARA-IN-TANCD    TO        STR-B97
           MOVE  SYS-DATE         TO        STR-B98
           MOVE  WK-TIME(1:6)     TO        STR-B99
     END-IF.
     IF    DSP-SYORI = "3"
           INITIALIZE                       STR-B00
           MOVE  STR-A01          TO        STR-B01
     END-IF.
*
* 履歴ファイル出力
     WRITE  STR-REC.
*
 FILPRT999.
     PERFORM  HEADDEL-SUB.
     MOVE  SPACE            TO  DSP-MEISAI.
     PERFORM  BODYDEL-SUB.
     MOVE  "2"              TO  MAIN-FLG.

 FILPRT-END.
     EXIT.
*--------------------------------------------------------------*
*    商品変換テーブル読込                                      *
*--------------------------------------------------------------*
 HSHOTBL-READ-SEC       SECTION.
*
     READ  HSHOTBL
           INVALID      MOVE "INV" TO HSHOTBL-INV-FLG
           NOT INVALID  MOVE SPACE TO HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込                                       *
*--------------------------------------------------------------*
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV" TO HMEIMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    サブ商品名称マスタ読込
*--------------------------------------------------------------*
 SUBMEIF-READ-SEC        SECTION.
*
     READ  SUBMEIF
           INVALID      MOVE "INV" TO SUBMEIF-INV-FLG
           NOT INVALID  MOVE SPACE TO SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    取引先マスタ読込                                          *
*--------------------------------------------------------------*
 HTOKMS-READ-SEC        SECTION.
*
     READ  HTOKMS
           INVALID      MOVE "INV" TO HTOKMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    倉庫マスタ読込　　                                        *
*--------------------------------------------------------------*
 ZSOKMS-READ-SEC        SECTION.
*
     READ  ZSOKMS
           INVALID      MOVE "INV" TO ZSOKMS-INV-FLG
           NOT INVALID  MOVE SPACE TO ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    サブ商品変換ＴＢＬ読込                                    *
*--------------------------------------------------------------*
 SUBTBLF-READ-SEC       SECTION.
*
     READ  SUBTBLF
           INVALID      MOVE "INV" TO SUBTBLF-INV-FLG
           NOT INVALID  MOVE SPACE TO SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
*#2020/12/15 NAV ST
*--------------------------------------------------------------*
*    条件ファイル読込　　　　                                  *
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*
     READ  HJYOKEN
           INVALID      MOVE "INV" TO HJYOKEN-INV-FLG
           NOT INVALID  MOVE SPACE TO HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*#2020/12/15 NAV ED
*--------------------------------------------------------------*
*    商品変換ＴＢＬより履歴（前）セット                        *
*--------------------------------------------------------------*
 TBL-TO-RIREKI-SET-SEC  SECTION.
*
        INITIALIZE                   STR-A00.
*取引先ＣＤ
        MOVE  TBL-F01            TO  STR-A01.
*量販店商品ＣＤ
        MOVE  TBL-F02            TO  STR-A02.
*サカタ商品ＣＤ
        MOVE  TBL-F031           TO  STR-A031.
*サカタ品単ＣＤ
        MOVE  TBL-F0321          TO  STR-A0321.
        MOVE  TBL-F0322          TO  STR-A0322.
        MOVE  TBL-F0323          TO  STR-A0323.
*出荷場所
        MOVE  TBL-F04            TO  STR-A04.
*原価単価
        MOVE  TBL-F05            TO  STR-A05.
*売価単価
        MOVE  TBL-F06            TO  STR-A06.
*分類
        MOVE  TBL-F07            TO  STR-A07.
*_番
        MOVE  TBL-F08            TO  STR-A08.
*仕入単価
        MOVE  TBL-F09            TO  STR-A09.
*張替区分
        MOVE  TBL-F10            TO  STR-A10.
*検品Ｇ
        MOVE  TBL-F11            TO  STR-A11.
*出荷地域区分
        MOVE  TBL-F12            TO  STR-A12.
*登録日付
        MOVE  TBL-F98            TO  STR-A94.
*更新日付
        MOVE  TBL-F99            TO  STR-A98.
*
 TBL-TO-RIREKI-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    サブ商品変換ＴＢＬより履歴（前）セット                    *
*--------------------------------------------------------------*
 SUB-TO-RIREKI-SET-SEC       SECTION.
*
        INITIALIZE                   STR-A00.
*取引先ＣＤ
        MOVE  SUB-F01            TO  STR-A01.
*量販店商品ＣＤ
        MOVE  SUB-F02            TO  STR-A02.
*サカタ商品ＣＤ
        MOVE  SUB-F031           TO  STR-A031.
*サカタ品単ＣＤ
        MOVE  SUB-F0321          TO  STR-A0321.
        MOVE  SUB-F0322          TO  STR-A0322.
        MOVE  SUB-F0323          TO  STR-A0323.
*出荷場所
        MOVE  SUB-F04            TO  STR-A04.
*原価単価
        MOVE  SUB-F05            TO  STR-A05.
*売価単価
        MOVE  SUB-F06            TO  STR-A06.
*分類
        MOVE  SUB-F07            TO  STR-A07.
*_番
        MOVE  SUB-F08            TO  STR-A08.
*仕入単価
        MOVE  SUB-F09            TO  STR-A09.
*張替区分
        MOVE  SUB-F10            TO  STR-A10.
*検品Ｇ
        MOVE  SUB-F11            TO  STR-A11.
*出荷地域区分
        MOVE  SUB-F12            TO  STR-A12.
*予備ＪＡＮＣＤ
        MOVE  SUB-F13            TO  STR-A13.
*予備インストアＣＤ
        MOVE  SUB-F14            TO  STR-A14.
*入数
        MOVE  SUB-F15            TO  STR-A15.
*振分倉庫
        MOVE  SUB-F16            TO  STR-A16.
*D365JANCD
        MOVE  SUB-F17            TO  STR-A17.
*D365商品CD
        MOVE  SUB-F18            TO  STR-A18.
*直送/セット組区分
        MOVE  SUB-F19            TO  STR-A19.
*登録担当者部門～更新時刻
     IF    DSP-SYORI  =  2  OR  3
           MOVE  SUB-F92         TO  STR-A92
           MOVE  SUB-F93         TO  STR-A93
           MOVE  SUB-F94         TO  STR-A94
           MOVE  SUB-F95         TO  STR-A95
           MOVE  SUB-F96         TO  STR-A96
           MOVE  SUB-F97         TO  STR-A97
           MOVE  SUB-F98         TO  STR-A98
           MOVE  SUB-F99         TO  STR-A99
     END-IF.
*
 SUB-TO-RIREKI-SET-EXIT.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  DSPF.
     CLOSE  SUBTBLF  HSHOTBL.
     CLOSE  HTOKMS   HMEIMS  ZSOKMS.
*#2020/12/15 NAV ST
     CLOSE  HJYOKEN.
*#2020/12/15 NAV ED
*
     DISPLAY "SUBTBLF ADD CNT = " SUB-ADD-CNT    UPON CONS.
     DISPLAY "SUBTBLF UPD CNT = " SUB-UPD-CNT    UPON CONS.
     DISPLAY "SUBTBLF DEL CNT = " SUB-DEL-CNT    UPON CONS.
     DISPLAY "HSHOTBL ADD CNT = " TBL-ADD-CNT    UPON CONS.
     DISPLAY "HSHOTBL UPD CNT = " TBL-UPD-CNT    UPON CONS.
     DISPLAY "HSHOTBL DEL CNT = " TBL-DEL-CNT    UPON CONS.
*
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
