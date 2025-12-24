# HGLOGIN

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/HGLOGIN.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＩＴ統制　　　　　　　　　　　　　*
*    モジュール名　　　　：　ログイン画面　　　　　　　　　　　*
*    作成日／更新日　　　：　09/03/16                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ログインユーザーＩＤ・ログインパス*
*                            ワードを入力制御を行なう。　　　　*
*    更新日　　　　　　　：　2013/01/16 NAV TAKAHASHI          *
*    　　　　　　　　　　：　パスワード入力文字数チェック変更  *
*    　　　　　　　　　　：　（６桁以上→８桁）　　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            HGLOGIN.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
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
*---<<  ログイン管理マスタ  >>---*
     SELECT   JHMLOGF   ASSIGN    TO        DA-01-VI-JHMLOGL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   LOG-F01
                        FILE      STATUS    IS   LOG-STATUS.
*---<<  担当者マスタ  >>---*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01  TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01  JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<<ログインユーザーファイル>>---*
     SELECT   LOGINUSR  ASSIGN    TO        DA-01-S-LOGINUSR
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   USR-STATUS
                        ORGANIZATION        IS   SEQUENTIAL.
*
******************************************************************
 DATA                   DIVISION.
******************************************************************
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FHGLOGIN  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  ログイン管理マスタ  >>---*
 FD  JHMLOGF.
     COPY     JHMLOGF   OF        XFDLIB
              JOINING   LOG       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<ログインユーザーファイル>>---*
 FD  LOGINUSR
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  LOGINUSR-REC.
     03  LOGINUSR-F01             PIC  X(08).
     03  LOGINUSR-F02             PIC  X(04).
     03  LOGINUSR-F03             PIC  X(08).
******************************************************************
*     作業領域
******************************************************************
 WORKING-STORAGE             SECTION.
*ログイン管理マスタ退避
     COPY   JHMLOGF  OF XFDLIB  JOINING   WRK  AS   PREFIX.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  LOG-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
     03  USR-STATUS          PIC  X(02).
****  カウンター
 01  WK-COUNTER.
     03  ADD-CNT             PIC  9(07)  VALUE ZERO.
     03  UPD-CNT             PIC  9(07)  VALUE ZERO.
     03  DEL-CNT             PIC  9(07)  VALUE ZERO.
     03  IX                  PIC  9(01)  VALUE ZERO.
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     03  HTANMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  JHMLOGF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  WK-AREA.
     03  WK-SYORI            PIC  9(01)  VALUE ZERO.
     03  OLD-PASS            PIC  X(08)  VALUE SPACE.
     03  OLD-PASS1           PIC  X(08)  VALUE SPACE.
     03  OLD-PASS2           PIC  X(08)  VALUE SPACE.
     03  OLD-PASS3           PIC  X(08)  VALUE SPACE.
     03  OLD-CHGKBN          PIC  X(01)  VALUE SPACE.
     03  OLD-ENDDAT          PIC  9(08)  VALUE ZERO.
     03  WK-PASS             PIC  X(08)  VALUE SPACE.
     03  FILLER              REDEFINES   WK-PASS.
         05  PASS-WK         PIC  X(01)  OCCURS  8.
     03  WK-MOJI             PIC  X(01)  VALUE SPACE.
     03  WK-SUUJI            PIC  X(01)  VALUE SPACE.
     03  WK-LOCK             PIC  9(01)  VALUE ZERO.
*ロックアウト回数
 01  WK-LOCK-COUNTER         PIC  9(01)  VALUE 3.
*パスワード（マスタ登録）
 01  WK-OK-PASS1             PIC  X(08)  VALUE SPACE.
*パスワード（変更パスワード）
 01  WK-OK-PASS2             PIC  X(08)  VALUE SPACE.
*パスワード（確認パスワード）
 01  WK-OK-PASS3             PIC  X(08)  VALUE SPACE.
*パスワード（最終）
 01  WK-OK-PASSWD            PIC  X(08)  VALUE SPACE.
*利用開始日／終了日
 01  WK-RIYO.
     03  WK-RIYO-START       PIC  9(08)  VALUE  ZERO.
     03  WK-RIYO-END         PIC  9(08)  VALUE  ZERO.
*メッセージ編集
 01  WK-MSG.
     03  WK-MSG02.
         05  WK-MSG02-1      PIC  N(15).
         05  WK-MSG02-2      PIC  N(15).
     03  WK-MSG03.
         05  WK-MSG03-1      PIC  N(15).
         05  WK-MSG03-2      PIC  N(15).
     03  WK-MSG04.
         05  WK-MSG04-1      PIC  N(15).
         05  WK-MSG04-2      PIC  N(15).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS               PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y            PIC  9(02)  VALUE  ZERO.
         05  WK-M            PIC  9(02)  VALUE  ZERO.
         05  WK-D            PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2  REDEFINES  DATE-AREA.
     03  SYS-DATE            PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY       PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD         PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM         PIC  9(02)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS         PIC  9(02)  VALUE  ZERO.
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "HGLOGIN".
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
            NC"既に登録済です。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"ユーザーＩＤが未登録です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"ユーザーＩＤを入力して下さい。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"パスワードを入力して下さい。".
*2013/01/16 NAV ST ６桁以上→８桁へ変更
     02  MSG-ERR6            PIC  N(28) VALUE
*********NC"６桁以上でないか先頭より入力が開始されていません".
         NC"８桁でないか先頭より入力が開始されていません".
*2013/01/16 NAV ED ６桁以上→８桁へ変更
     02  MSG-ERR7            PIC  N(28) VALUE
         NC"パスワードは数字文字混在で設定して下さい。".
     02  MSG-ERR8            PIC  N(28) VALUE
         NC"過去３回のパスワードで同一のパスワードがあります。".
     02  MSG-ERR9            PIC  N(28) VALUE
         NC"登録されているパスワードと違います。".
     02  MSG-ERR10           PIC  N(28) VALUE
         NC"パスワード　再入力エラーです？？　確認必要！".
     02  MSG-ERR11           PIC  N(28) VALUE
       NC"パスワードが、現行と同一です。別パスワードを設定！！".
     02  MSG-ERR12           PIC  N(28) VALUE
       NC"利用可能期間外！！　終了してＩＴ統制担当者に連絡！！".
     02  MSG-ERR13           PIC  N(28) VALUE
         NC"パスワードがロックアウトされました。".
     02  MSG-ERR14           PIC  N(28) VALUE
         NC"　　　　　　　　　　　".
     02  MSG-ERR15           PIC  N(28) VALUE
         NC"　　　　　　　　　　　".
     02  MSG-ERR16           PIC  N(28) VALUE
         NC"Ｙを入力して下さい。".
     02  MSG-ERR17           PIC  N(28) VALUE
         NC"　　　　　　　　　　　".
 01  ERR-MSG-ALL             REDEFINES   ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  17  TIMES.
*３ヶ月経過判定
 01  WK-KEIKA.
     03  WK-KEIKA-YYYY       PIC  9(04)  VALUE  ZERO.
     03  WK-KEIKA-MM         PIC  9(02)  VALUE  ZERO.
     03  WK-KEIKA-DD         PIC  9(02)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
******************************************************************
 01  XTWAIT-TIME             PIC  9(06)  BINARY.
 01  XTWAIT-WORK             PIC  X(64).
 LINKAGE                     SECTION.
 01  PARA-USERID             PIC  X(08).
 01  PARA-BUMON              PIC  X(04).
 01  PARA-TANCD              PIC  X(02).
 01  PARA-ST                 PIC  X(02).
******************************************************************
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING  PARA-USERID
                                              PARA-BUMON
                                              PARA-TANCD
                                              PARA-ST.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     MOVE     "04"           TO   PARA-ST.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JHMLOGF.
     MOVE     "JHMLOGL1"     TO   ERR-FL-ID.
     MOVE     LOG-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     MOVE     "04"           TO   PARA-ST.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTANMS.
     MOVE     "TANMS1  "     TO   ERR-FL-ID.
     MOVE     TAN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     MOVE     "04"           TO   PARA-ST.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1 "     TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     MOVE     "04"           TO   PARA-ST.
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    LOGINUSR.
     MOVE     "LOGINUSR"     TO   ERR-FL-ID.
     MOVE     USR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     MOVE     "04"           TO   PARA-ST.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      1.0　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             DSPF.
     OPEN    I-O             JHMLOGF.
     OPEN    INPUT           HTANMS  HJYOKEN.
     OPEN    OUTPUT          LOGINUSR.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*初期化
     MOVE    ZERO                 TO   WK-LOCK.
     MOVE    "00"                 TO   PARA-ST.
*初期画面表示へ
     MOVE    "0"                  TO   MAIN-FLG.
*
 INIT-END.
     EXIT.
****************************************************************
*      2.0　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "0"       PERFORM   DSP-INIT-SUB
         WHEN      "1"       PERFORM   USERID-SUB
         WHEN      "2"       PERFORM   PASSWD-SUB
         WHEN      "3"       PERFORM   KAKUNIN-SUB
         WHEN      "4"       PERFORM   PASSWD-KAKU1-SUB
         WHEN      "5"       PERFORM   PASSWD-KAKU2-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    DSPF  JHMLOGF  HTANMS  HJYOKEN  LOGINUSR.
*
 END-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1     初期画面表示
*--------------------------------------------------------------*
 DSP-INIT-SUB                SECTION.
*画面初期化
     MOVE    SPACE           TO   DSP-CONTROL.
     MOVE    SPACE           TO   DSP-FHGLOGIN.
     MOVE    "FHGLOGIN"      TO   DSP-FORMAT.
*初期化
     PERFORM GRPUSRDEL-SUB.
     PERFORM GRPPASDEL-SUB.
     PERFORM GRPPS1DEL-SUB.
     PERFORM GRPPS2DEL-SUB.
*
     PERFORM DSP-WRITE-SUB.
*
     MOVE    "1"             TO   MAIN-FLG.
*
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2       ユーザーＩＤ入力
*--------------------------------------------------------------*
 USERID-SUB                  SECTION.
*
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           MOVE   "0"        TO   MAIN-FLG
           MOVE   ZERO       TO   ERR-MSG-CD
       WHEN
        "F005"
           MOVE   "END"      TO   END-FLG
           MOVE   "01"       TO   PARA-ST
       WHEN
        "F006"
           MOVE   "0"        TO   MAIN-FLG
           MOVE   ZERO       TO   ERR-MSG-CD
           PERFORM GRPUSRCLR-SUB
           PERFORM GRPPASCLR-SUB
           PERFORM GRPPS1CLR-SUB
           PERFORM GRPPS2CLR-SUB
       WHEN
        "E000"
           PERFORM    USERID-CHK-SUB
                  IF  ERR-MSG-CD   =    ZERO
                      MOVE   "2"   TO   MAIN-FLG
                      MOVE "D"   TO EDIT-OPTION OF DSP-USERID
                      MOVE SPACE TO EDIT-CURSOR OF DSP-USERID
                      PERFORM GRPUSRCLR-SUB
                      PERFORM PASSWD-MSG-SUB
                      MOVE  DSP-USERID          TO PARA-USERID
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 USERID-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ユーザーＩＤのチェック
*--------------------------------------------------------------*
 USERID-CHK-SUB         SECTION.
*
     IF  DSP-USERID     =    SPACE
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-USERID
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-USERID
     ELSE
         PERFORM   FILCHK-SUB
     END-IF.
*
 USERID-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   ファイルの存在チェック
*--------------------------------------------------------------*
 FILCHK-SUB             SECTION.
*
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-USERID      TO   LOG-F01.
     PERFORM JHMLOGF-READ-SUB.
*
* ＩＮＶＡＬＩＤ処理
     IF  JHMLOGF-INV-FLG  =  "INV"
         MOVE   03      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-USERID
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-USERID
         GO             TO   FILCHK-END
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
*    利用開始日チェック
     MOVE   LOG-F05     TO   WK-RIYO-START.
     IF     LOG-F06  NOT NUMERIC
     OR     LOG-F06  =   ZERO
            MOVE 99999999 TO WK-RIYO-END
     ELSE
            MOVE LOG-F06  TO WK-RIYO-END
     END-IF.
*
     IF  WK-RIYO-START   <=  DATE-AREA
     AND WK-RIYO-END     >=  DATE-AREA
         CONTINUE
     ELSE
         MOVE   12      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-USERID
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-USERID
         GO             TO   FILCHK-END
     END-IF.
*
     PERFORM        FILE-SUB.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-USERID.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-USERID.
*
 FILCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイルセット                               *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
*ワーク初期化
     MOVE    SPACE            TO   WRK-REC.
     INITIALIZE                    WRK-REC.
*レコード退避
     MOVE    LOG-REC          TO   WRK-REC.
*パスワード
     MOVE    LOG-F02          TO   OLD-PASS.
*部門ＣＤ
     MOVE    LOG-F03          TO   DSP-BUMON  PARA-BUMON.
*部門名称取得
     MOVE    "22"             TO   JYO-F01.
     MOVE    LOG-F03          TO   JYO-F02.
     PERFORM HJYOKEN-READ-SUB.
     IF      HJYOKEN-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-BUMNM
     ELSE
             MOVE  JYO-F03    TO   DSP-BUMNM
     END-IF.
*担当者ＣＤ
     MOVE    LOG-F04          TO   DSP-TANCD  PARA-TANCD.
*担当者名取得
     MOVE    LOG-F03          TO   TAN-F01.
     MOVE    LOG-F04          TO   TAN-F02.
     PERFORM HTANMS-READ-SUB
     IF      HTANMS-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-TANNM
     ELSE
             MOVE  TAN-F03    TO   DSP-TANNM
     END-IF.
*過去登録ＩＤの退避
     MOVE    LOG-F10          TO   OLD-PASS1.
     MOVE    LOG-F11          TO   OLD-PASS2.
     MOVE    LOG-F12          TO   OLD-PASS3.
*利用開始日セット
*以降処理の判定
*初回ＩＤ変更区分
     IF      LOG-F07  =  "1"
*************３ヶ月経過チェック
             MOVE  LOG-F09    TO   WK-KEIKA
             ADD   3          TO   WK-KEIKA-MM
             IF    WK-KEIKA-MM > 12
                   COMPUTE WK-KEIKA-MM = WK-KEIKA-MM - 12
                   ADD     1  TO   WK-KEIKA-YYYY
             END-IF
*************３ヶ月経過判定
             IF    WK-KEIKA < DATE-AREA
                   MOVE  "3"  TO   WK-SYORI
             ELSE
                   MOVE  "1"  TO   WK-SYORI
             END-IF
      ELSE
             MOVE        "4"  TO   WK-SYORI
      END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力                               *
*--------------------------------------------------------------*
 PASSWD-SUB        SECTION.
*
 PASSWD-010.
     PERFORM       DSP-READ-SUB.
 PASSWD-020.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  MOVE   "0"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F005"
                  MOVE   "END"        TO   END-FLG
                  MOVE   "01"         TO   PARA-ST
        WHEN  "F006"
                  MOVE   "1"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
                  PERFORM  GRPUSRCLR-SUB
                  PERFORM  GRPPASCLR-SUB
                  PERFORM  GRPPS1CLR-SUB
                  PERFORM  GRPPS2CLR-SUB
        WHEN  "E000"
               PERFORM        PASSCHK-SUB
               IF   ERR-MSG-CD      =   ZERO
                    IF    WK-SYORI  =   1  OR  2
                          MOVE   "3"    TO  MAIN-FLG
                          MOVE   DSP-PASSWD TO  WK-OK-PASSWD
                    ELSE
                          MOVE   "4"    TO  MAIN-FLG
                    END-IF
                    MOVE  DSP-PASSWD  TO  WK-OK-PASS1
                    MOVE  ALL "*"     TO  DSP-PASSWD
                    MOVE "D"   TO EDIT-OPTION OF DSP-PASSWD
                    MOVE SPACE TO EDIT-CURSOR OF DSP-PASSWD
                    PERFORM GRPPASCLR-SUB
               END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 PASSWD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.1     パスワードチェック　　　　　                  *
*--------------------------------------------------------------*
 PASSCHK-SUB            SECTION.
*パスワード入力
     IF  DSP-PASSWD  =  SPACE
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASSWD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASSWD
         GO             TO   PASSCHK-010
     END-IF.
*ログイン管理マスタに登録されているパスワードと同一かチェック
     IF  DSP-PASSWD   NOT =  OLD-PASS
         MOVE   09      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASSWD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASSWD
         ADD     1      TO   WK-LOCK
         MOVE WK-LOCK   TO   DSP-LCCNT
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-PASSWD
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-PASSWD
         MOVE   ZERO    TO   WK-LOCK          DSP-LCCNT
     END-IF.
 PASSCHK-010.
*ロックアウトチェック
     IF  WK-LOCK  >=  WK-LOCK-COUNTER
         PERFORM  FILE-LOCKOUT-SUB
         MOVE   13      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASSWD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASSWD
     END-IF.
*
 PASSCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.1.1    修正
*--------------------------------------------------------------*
 FILE-LOCKOUT-SUB       SECTION.
*ファイル索引
     MOVE    DSP-USERID      TO   LOG-F01.
     PERFORM JHMLOGF-READ-SUB.
* ＩＮＶＡＬＩＤ処理
     IF  JHMLOGF-INV-FLG  =  "INV"
         DISPLAY NC"＃更新エラーです。＃"  UPON CONS
         DISPLAY NC"＃更新対象がありま＃"  UPON CONS
         DISPLAY NC"＃せん。確認して下＃"  UPON CONS
         DISPLAY NC"＃さい　　　　　　＃"  UPON CONS
         STOP  RUN
     END-IF.
*更新日付
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
     MOVE      LINK-OUT-YMD       TO   LOG-F98.
*ロックアウト回数
     MOVE      ZERO           TO       LOG-F13.
*ロックアウト日付
     MOVE      ZERO           TO       LOG-F14.
*ロックアウト時間
     MOVE      ZERO           TO       LOG-F15.
*更新担当者部門ＣＤ
     MOVE      WRK-F03        TO       LOG-F96.
*更新担当者ＣＤ
     MOVE      WRK-F04        TO       LOG-F97.
*更新時刻
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   LOG-F99.
*ファイル出力
     REWRITE   LOG-REC.
*
 FILE-LOCKOUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
*
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KKNN
        WHEN  "F005"
              MOVE   "END"        TO   END-FLG
              MOVE   "01"         TO   PARA-ST
        WHEN  "F006"
              MOVE    "2"         TO   MAIN-FLG
              MOVE    SPACE       TO   DSP-KKNN
              MOVE    SPACE       TO   DSP-PASSWD
              PERFORM GRPUSRCLR-SUB
              PERFORM GRPPASCLR-SUB
              PERFORM GRPPS1CLR-SUB
              PERFORM GRPPS2CLR-SUB
        WHEN  "F010"
**************パスワード変更画面へ
              MOVE   "4"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KKNN
              MOVE    2           TO   WK-SYORI
              PERFORM PASSWD-MSG-SUB
              PERFORM DSP-WRITE-SUB
        WHEN  "E000"
              PERFORM  KOUSIN-SUB
              MOVE "END"     TO   END-FLG
        WHEN  OTHER
              MOVE   01      TO   ERR-MSG-CD
     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4.1      ログイン管理マスタ更新
*--------------------------------------------------------------*
 KOUSIN-SUB             SECTION.
*
     PERFORM FILE-UPD-SUB.
*
 KOUSIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4.1.1    ログイン管理マスタ更新
*--------------------------------------------------------------*
 FILE-UPD-SUB           SECTION.
*ファイル索引
     MOVE    DSP-USERID      TO   LOG-F01.
     PERFORM JHMLOGF-READ-SUB.
* ＩＮＶＡＬＩＤ処理
     IF  JHMLOGF-INV-FLG  =  "INV"
         DISPLAY NC"＃更新エラーです。＃"  UPON CONS
         DISPLAY NC"＃更新対象がありま＃"  UPON CONS
         DISPLAY NC"＃せん。確認して下＃"  UPON CONS
         DISPLAY NC"＃さい　　　　　　＃"  UPON CONS
         STOP  RUN
     END-IF.
*パスワード
     MOVE        WK-OK-PASSWD TO       LOG-F02.
*更新日付
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
     MOVE      LINK-OUT-YMD       TO   LOG-F98.
*****パスワードが変更されていた場合
     IF        WK-OK-PASSWD  NOT =  OLD-PASS
******最終PASS変更日
               MOVE LOG-F98      TO      LOG-F09
******保存パスワード３
               MOVE LOG-F11      TO      LOG-F12
******保存パスワード２
               MOVE LOG-F10      TO      LOG-F11
******保存パスワード１
               MOVE WK-OK-PASSWD TO      LOG-F10
     END-IF.
*初期ＩＤ登録の場合
     IF   WK-SYORI  =  "4"
*****初期ＩＤ変更区分
          MOVE   "1"          TO       LOG-F07
*****初期ＩＤ変更日付
          MOVE    DATE-AREA   TO       LOG-F08
*****過去パスワード初期化
          MOVE    SPACE       TO       LOG-F10  LOG-F11  LOG-F12
*****最新パスワードセット
          MOVE   WK-OK-PASSWD TO       LOG-F10
     END-IF.
*ロックアウト回数
     MOVE      ZERO           TO       LOG-F13.
*ロックアウト日付
     MOVE      ZERO           TO       LOG-F14.
*ロックアウト時間
     MOVE      ZERO           TO       LOG-F15.
*更新担当者部門ＣＤ
     MOVE      WRK-F03        TO       LOG-F96.
*更新担当者ＣＤ
     MOVE      WRK-F04        TO       LOG-F97.
*更新時刻
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   LOG-F99.
*ファイル出力
     REWRITE   LOG-REC.
*件数カウント
     ADD       1              TO       UPD-CNT.
*ログインユーザーファイル作成
*初期化
     MOVE      SPACE          TO       LOGINUSR-REC.
     INITIALIZE                        LOGINUSR-REC.
*項目セット
     MOVE      DSP-USERID     TO       LOGINUSR-F01.
     MOVE      DSP-BUMON      TO       LOGINUSR-F02.
     MOVE      DSP-TANCD      TO       LOGINUSR-F03(1:2).
*レコード作成
     WRITE     LOGINUSR-REC.
*
 FILE-UPD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        パスワード変更画面
*--------------------------------------------------------------*
 PASSWD-KAKU1-SUB  SECTION.
*
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  MOVE   "0"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F005"
                  MOVE   "END"        TO   END-FLG
                  MOVE   "01"         TO   PARA-ST
        WHEN  "F006"
                  IF     WK-SYORI  =  1  OR  2
                         MOVE  "3"    TO   MAIN-FLG
                  ELSE
                         MOVE  "2"    TO   MAIN-FLG
                  END-IF
                  PERFORM GRPPS1CLR-SUB
                  PERFORM GRPPS2CLR-SUB
                  MOVE   SPACE        TO   DSP-PSAREA
        WHEN  "E000"
                  PERFORM        PASSCHK1-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE  "5"         TO  MAIN-FLG
                       MOVE  DSP-PASSWD  TO  WK-OK-PASSWD
                       MOVE  DSP-PASS1   TO  WK-OK-PASS2
                       MOVE  ALL "*"     TO  DSP-PASS1
                       PERFORM GRPPS1CLR-SUB
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 PASSWD-KAKU1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.5.1     変更パスワード変更チェック
*--------------------------------------------------------------*
 PASSCHK1-SUB           SECTION.
*
     MOVE       ZERO    TO   ERR-MSG-CD.
*パスワード入力
     IF  DSP-PASS1  =  SPACE
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS1
         GO             TO   PASSCHK1-END
     END-IF.
*前回パスワードチェック
     IF  DSP-PASS1  =  WK-OK-PASS1
         MOVE   11      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS1
         GO             TO   PASSCHK1-END
     END-IF.
*６桁以上かチェック
     IF  DSP-PASS1(1:1) NOT = SPACE
     AND DSP-PASS1(2:1) NOT = SPACE
     AND DSP-PASS1(3:1) NOT = SPACE
     AND DSP-PASS1(4:1) NOT = SPACE
     AND DSP-PASS1(5:1) NOT = SPACE
     AND DSP-PASS1(6:1) NOT = SPACE
*2013/01/16 NAV ST ７、８桁の入力もチェック
     AND DSP-PASS1(7:1) NOT = SPACE
     AND DSP-PASS1(8:1) NOT = SPACE
*2013/01/16 NAV ED ７、８桁の入力もチェック
         CONTINUE
     ELSE
         MOVE   06      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS1
         GO             TO   PASSCHK1-END
     END-IF.
*数字文字混在チェック
     MOVE    SPACE      TO   WK-PASS  WK-MOJI  WK-SUUJI.
     MOVE    DSP-PASS1  TO   WK-PASS.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 8
             IF PASS-WK(IX) NOT = SPACE
                IF  PASS-WK(IX)  NUMERIC
                    MOVE  "1"    TO   WK-SUUJI
                END-IF
                IF  PASS-WK(IX)  NOT NUMERIC
                    MOVE  "1"    TO   WK-MOJI
                END-IF
             END-IF
     END-PERFORM.
*
     IF  WK-MOJI  =  "1"
     AND WK-SUUJI =  "1"
         CONTINUE
     ELSE
         MOVE   07      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS1
         GO             TO   PASSCHK1-END
     END-IF.
*修正の時、過去３回パスワードと同一でないかチェック
     IF  OLD-PASS1  =  DSP-PASS1
     OR  OLD-PASS2  =  DSP-PASS1
     OR  OLD-PASS3  =  DSP-PASS1
         MOVE   08      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS1
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-PASS1
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-PASS1
     END-IF.
*
 PASSCHK1-END.
     EXIT.
*--------------------------------------------------------------*
*      2.6        変更パスワード確認画面
*--------------------------------------------------------------*
 PASSWD-KAKU2-SUB  SECTION.
*
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  MOVE   "0"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F005"
                  MOVE   "END"        TO   END-FLG
                  MOVE   "01"         TO   PARA-ST
        WHEN  "F006"
                  MOVE   "4"          TO   MAIN-FLG
                  PERFORM GRPPS2DEL-SUB
        WHEN  "E000"
                  PERFORM        PASSCHK2-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE  "3"         TO  MAIN-FLG
                       MOVE  DSP-PASS2   TO  WK-OK-PASSWD
                       MOVE  DSP-PASS2   TO  WK-OK-PASS3
                       MOVE  ALL "*"     TO  DSP-PASS2
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 PASSWD-KAKU2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.6.1      変更パスワード確認画面チェック
*--------------------------------------------------------------*
 PASSCHK2-SUB           SECTION.
*パスワード入力
     MOVE       ZERO    TO   ERR-MSG-CD.
     IF  WK-OK-PASS2  NOT =  DSP-PASS2
         MOVE   10      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS2
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS2
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-PASS2
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-PASS2
     END-IF.
*
 PASSCHK2-END.
     EXIT.
****************************************************************
*      ALL        担当者マスタ読み込み
****************************************************************
 HTANMS-READ-SUB        SECTION.
*
     READ  HTANMS
           INVALID      MOVE  "INV"    TO   HTANMS-INV-FLG
           NOT  INVALID MOVE  SPACE    TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        ログイン管理マスタ　読み込み
****************************************************************
 JHMLOGF-READ-SUB       SECTION.
*
     READ  JHMLOGF
           INVALID      MOVE  "INV"    TO   JHMLOGF-INV-FLG
           NOT  INVALID MOVE  SPACE    TO   JHMLOGF-INV-FLG
     END-READ.
*
 JHMLOGF-READ-EXIT.
     EXIT.
****************************************************************
*      ALL        条件ファイル　読み込み
****************************************************************
 HJYOKEN-READ-SUB       SECTION.
*
     READ  HJYOKEN
           INVALID      MOVE  "INV"    TO   HJYOKEN-INV-FLG
           NOT  INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    （共通）画面表示処理
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
*
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     WRITE    DSP-FHGLOGIN.
*
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*     （共通）画面読み込み
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
*エラーメッセージ表示
     IF       ERR-MSG-CD   =    0
              MOVE      SPACE    TO   DSP-MSG01
              MOVE      "D"      TO   EDIT-OPTION OF DSP-MSG01
              MOVE      "R"      TO   EDIT-COLOR  OF DSP-MSG01
     ELSE
              MOVE      ERR-MSG (ERR-MSG-CD)   TO   DSP-MSG01
              MOVE      "M"      TO   EDIT-OPTION OF DSP-MSG01
              MOVE      " "      TO   EDIT-COLOR  OF DSP-MSG01
     END-IF.
*下記エラーの場合、１０秒後に終了メッセージを出力する。
     IF     ERR-MSG-CD  =  12  OR  13
            MOVE  NC"　　　　　《１０秒後に強制終了されます。》"
                  TO    DSP-MSG02
     END-IF.
*画面ＷＲＩＴＥ
     PERFORM DSP-WRITE-SUB.
*下記エラーの場合、１０秒後にプログラムを終了する。
     EVALUATE ERR-MSG-CD
         WHEN 12    MOVE   "03"     TO   PARA-ST
                    PERFORM TWAIT-SUB
                    STOP  RUN
         WHEN 13    MOVE   "02"     TO   PARA-ST
                    PERFORM TWAIT-SUB
                    STOP  RUN
     END-EVALUATE.
*****IF     ERR-MSG-CD  =  12  OR  13
*           PERFORM  TWAIT-SUB
*           STOP  RUN
*****END-IF.
*
     MOVE   SPACE       TO   DSP-MSG01.
*
     MOVE  "NE"         TO   DSP-PROC.
*ＰＦガイドセット
     EVALUATE      MAIN-FLG
         WHEN      "1"  MOVE  "GRPUSR" TO   DSP-GROUP
         WHEN      "2"  MOVE  "GRPPAS" TO   DSP-GROUP
         WHEN      "3"  MOVE  "KAKU  " TO   DSP-GROUP
         WHEN      "4"  MOVE  "GRPPA1" TO   DSP-GROUP
         WHEN      "5"  MOVE  "GRPPA2" TO   DSP-GROUP
     END-EVALUATE.
     MOVE   "FHGLOGIN"                 TO   DSP-FORMAT.
     READ   DSPF.
*
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*    ALL   ユーザーＩＤ初期化
*--------------------------------------------------------------*
 GRPUSRDEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP001.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-USERID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-USERID.
*
 GRPUSRDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL   パスワード初期化
*--------------------------------------------------------------*
 GRPPASDEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP002.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASSWD.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASSWD.
*
 GRPPASDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL  変更パスワード初期化
*--------------------------------------------------------------*
 GRPPS1DEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP003.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS1.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS1.
*
 GRPPS1DEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL  変更パスワード確認初期化
*--------------------------------------------------------------*
 GRPPS2DEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP004.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS2.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS2.
*
 GRPPS2DEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL    ユーザーＩＤ初期化
*--------------------------------------------------------------*
 GRPUSRCLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-USERID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-USERID.
*
 GRPUSRCLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL  パスワード初期化
*--------------------------------------------------------------*
 GRPPASCLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASSWD.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASSWD.
*
 GRPPASCLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL  パスワード初期化
*--------------------------------------------------------------*
 GRPPS1CLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS1.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS1.
*
 GRPPS1CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL   パスワード初期化
*--------------------------------------------------------------*
 GRPPS2CLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS2.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS2.
*
 GRPPS2CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ALL   メッセージ出力
*--------------------------------------------------------------*
 PASSWD-MSG-SUB          SECTION.
*
     MOVE       SPACE    TO     WK-MSG.
     EVALUATE   WK-SYORI
     WHEN   2
     MOVE NC"変更するパスワードを入力して下" TO WK-MSG03-1
     MOVE NC"さい。過去３回のパスワードの使" TO WK-MSG03-2
*2013/01/16 NAV ST ６桁以上→８桁に変更
*****MOVE NC"用は出来ません。パスワードは６" TO WK-MSG04-1
*****MOVE NC"桁以上・英字数字混在で入力！！" TO WK-MSG04-2
     MOVE NC"用は出来ません。パスワードは８" TO WK-MSG04-1
     MOVE NC"桁・英字数字混在で入力！！　　" TO WK-MSG04-2
*2013/01/16 NAV ED ６桁以上→８桁に変更
     MOVE NC"　　　＜変更パスワード入力＞⇒" TO DSP-MSG051
     MOVE NC"　　　＜変更パスワード確認＞⇒" TO DSP-MSG061
     MOVE WK-MSG03                           TO DSP-MSG03
     MOVE WK-MSG04                           TO DSP-MSG04
     WHEN   3
     MOVE NC"前回パスワード変更より、３ヶ月" TO WK-MSG03-1
     MOVE NC"経過しております。規定によりパ" TO WK-MSG03-2
*2013/01/16 NAV ST ６桁以上→８桁に変更
*****MOVE NC"スワードを変更して下さい。　６" TO WK-MSG04-1
*****MOVE NC"桁以上・英字数字混在で入力！！" TO WK-MSG04-2
     MOVE NC"スワードを変更して下さい。　８" TO WK-MSG04-1
     MOVE NC"桁・英字数字混在で入力！！　　" TO WK-MSG04-2
*2013/01/16 NAV ED ６桁以上→８桁に変更
     MOVE NC"　　　＜変更パスワード入力＞⇒" TO DSP-MSG051
     MOVE NC"　　　＜変更パスワード確認＞⇒" TO DSP-MSG061
     MOVE WK-MSG03                           TO DSP-MSG03
     MOVE WK-MSG04                           TO DSP-MSG04
     WHEN   4
     MOVE NC"＜初回ログインの場合、必ずパス" TO WK-MSG02-1
     MOVE NC"ワードを変更して下さい＞　　　" TO WK-MSG02-2
     MOVE NC"初回パスワードを変更して下さい" TO WK-MSG03-1
     MOVE NC"。　　　　　　　　　　　　　　" TO WK-MSG03-2
*2013/01/16 NAV ST ６桁以上→８桁に変更
*****MOVE NC"パスワードは６桁以上で、英字数" TO WK-MSG04-1
*****MOVE NC"字混在で入力して下さい。　　　" TO WK-MSG04-2
     MOVE NC"パスワードは８桁で、英字数字混" TO WK-MSG04-1
     MOVE NC"在で入力して下さい。　　　　　" TO WK-MSG04-2
*2013/01/16 NAV ED ６桁以上→８桁に変更
     MOVE NC"　　　＜変更パスワード入力＞⇒" TO DSP-MSG051
     MOVE NC"　　　＜変更パスワード確認＞⇒" TO DSP-MSG061
     MOVE WK-MSG02                           TO DSP-MSG02
     MOVE WK-MSG03                           TO DSP-MSG03
     MOVE WK-MSG04                           TO DSP-MSG04
     END-EVALUATE.
*
 PASSWD-MSG-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    １０秒間待ち
*--------------------------------------------------------------*
 TWAIT-SUB               SECTION.
*
     MOVE         10         TO          XTWAIT-TIME.
     CALL         "XSBTWAIT" USING       XTWAIT-TIME
                                         XTWAIT-WORK.
*
 TWAIT-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
