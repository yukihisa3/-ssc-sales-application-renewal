# SIT0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0010I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＩＴ統制　　　　　　　　　　　　　*
*    モジュール名　　　　：　ログイン管理マスタ保守　　　　　　*
*    作成日／更新日　　　：　09/03/11                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　ログイン管理マスタの登録・修正・　*
*                            削除を行なう。　　　　　　　　　　*
*    更新日　　　　　　　：　2013/01/22 NAV TAKAHASHI          *
*    　　　　　　　　　　：　パスワードの桁数を６桁以上→８桁  *
*    更新日／更新者　　　：　2019/03/25  /INOUE                *
*             S11300660 マスタ更新履歴ファイル項目追加         *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0010I.
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
*マスタ更新履歴ファイル
     SELECT   MSTLOGF   ASSIGN    TO        DA-01-VI-MSTLOGL1
                           ORGANIZATION     IS   INDEXED
                           ACCESS    MODE   IS   DYNAMIC
                           RECORD    KEY    IS   MSL-F01
                                                 MSL-F02
                                                 MSL-F03
                                                 MSL-F04
                                                 MSL-F05
                                                 MSL-F06
                                                 MSL-F07
                           FILE      STATUS IS   MSL-STATUS.
*
******************************************************************
 DATA                   DIVISION.
******************************************************************
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FIT00101  OF        XMDLIB
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
*---<< マスタ更新履歴ファイル >>---*
 FD  MSTLOGF.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
******************************************************************
*     作業領域
******************************************************************
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
     03  LOG-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
     03  MSL-STATUS          PIC  X(02).
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
     03  OLD-CHGKBN          PIC  X(01)  VALUE SPACE.
     03  OLD-ENDDAT          PIC  9(08)  VALUE ZERO.
     03  WK-PASS             PIC  X(08)  VALUE SPACE.
     03  FILLER              REDEFINES   WK-PASS.
         05  PASS-WK         PIC  X(01)  OCCURS  8.
     03  WK-MOJI             PIC  X(01)  VALUE SPACE.
     03  WK-SUUJI            PIC  X(01)  VALUE SPACE.
     03  WK-SEQ              PIC  9(02)  VALUE ZERO.
*
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
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消　_終了".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_項目戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0010I".
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
     02  MSG-ERR6            PIC  N(28) VALUE
*2013/01/22 NAV ST ６桁以上→８桁
*********NC"６桁以上でないか先頭より入力が開始されていません".
         NC"８桁でないか先頭より入力が開始されていません".
*2013/01/22 NAV ED ６桁以上→８桁
     02  MSG-ERR7            PIC  N(28) VALUE
         NC"パスワードは数字文字混在で設定して下さい。".
     02  MSG-ERR8            PIC  N(28) VALUE
         NC"過去３回のパスワードで同一のパスワードがあります。".
     02  MSG-ERR9            PIC  N(28) VALUE
         NC"部門ＣＤを入力して下さい。".
     02  MSG-ERR10           PIC  N(28) VALUE
         NC"担当者ＣＤを入力して下さい。".
     02  MSG-ERR11           PIC  N(28) VALUE
         NC"利用開始日を入力して下さい。".
     02  MSG-ERR12           PIC  N(28) VALUE
         NC"日付論理エラーです。".
     02  MSG-ERR13           PIC  N(28) VALUE
         NC"開始が終了を超えています。".
     02  MSG-ERR14           PIC  N(28) VALUE
         NC"空白又は１以外は入力出来ません。".
     02  MSG-ERR15           PIC  N(28) VALUE
         NC"最終ＰＡＳＳ更新日は日付を入力して下さい。".
     02  MSG-ERR16           PIC  N(28) VALUE
         NC"Ｙを入力して下さい。".
     02  MSG-ERR17           PIC  N(28) VALUE
         NC"モード選択エラーです。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  17  TIMES.
*
*レコード退避エリア
*↓2019.03.25
 01  BEFORE-DATA           PIC  X(256).
*↑2019.03.25
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 LINKAGE                     SECTION.
 01  PARA-BUMON            PIC X(04).
 01  PARA-TANCD            PIC X(02).
 01  PARA-SDATE            PIC 9(08).
 01  PARA-STIME            PIC 9(06).
******************************************************************
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING  PARA-BUMON
                                              PARA-TANCD
                                              PARA-SDATE
                                              PARA-STIME.
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
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    MSTLOGF.
     MOVE     "MSTLOGL1"     TO   ERR-FL-ID.
     MOVE     MSL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
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
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             DSPF.
     OPEN    I-O             JHMLOGF.
     OPEN    I-O             MSTLOGF.
     OPEN    INPUT           HTANMS  HJYOKEN.
*権限判定
     MOVE    PARA-BUMON           TO   TAN-F01.
     MOVE    PARA-TANCD           TO   TAN-F02.
     PERFORM HTANMS-READ-SUB.
     IF      TAN-F07  NOT =  "1"
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃"   UPON  CONS
             DISPLAY NC"＃実行権限がありません＃"   UPON  CONS
             DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃"   UPON  CONS
             STOP  RUN
     END-IF.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA  PARA-SDATE.
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
     MOVE      WK-TIME(1:6)       TO   PARA-STIME.
*初期化
     MOVE    ZERO                 TO   WK-SEQ.
*
     MOVE    "0"                  TO   MAIN-FLG.
*
 INIT-END.
     EXIT.
****************************************************************
*      2.0　　メイン処理                                       *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "0"       PERFORM   DSP-INIT-SUB
         WHEN      "1"       PERFORM   SYORI-SUB
         WHEN      "2"       PERFORM   USERID-SUB
         WHEN      "3"       PERFORM   PASSWD-SUB
         WHEN      "4"       PERFORM   BODY-SUB
         WHEN      "5"       PERFORM   KAKUNIN-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
****************************************************************
*      2.1     初期画面表示
****************************************************************
 DSP-INIT-SUB                SECTION.
*画面初期化
     MOVE    SPACE           TO   DSP-CONTROL.
     MOVE    SPACE           TO   DSP-FIT00101.
     MOVE    "FIT00101"      TO   DSP-FORMAT.
*初期化
     PERFORM KEYDEL-SUB.
     PERFORM GRPUSRDEL-SUB.
     PERFORM GRPPASDEL-SUB.
     PERFORM BODYDEL-SUB.
*担当者ＣＤ
     MOVE    PARA-TANCD       TO   DSP-HTANCD.
*担当者名取得
     MOVE    PARA-BUMON       TO   TAN-F01.
     MOVE    PARA-TANCD       TO   TAN-F02.
     PERFORM HTANMS-READ-SUB
     IF      HTANMS-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-NTANNM
     ELSE
             MOVE  TAN-F03    TO   DSP-NTANNM
     END-IF.
*
     PERFORM DSP-WRITE-SUB.
*
     MOVE    "1"             TO   MAIN-FLG.
*
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.1       処理区分入力                                  *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
*エラーメッセージ番号初期化
     MOVE    "KEY"           TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           MOVE   "0"          TO   MAIN-FLG
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
           PERFORM        SYORICHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         MOVE   "2"   TO   MAIN-FLG
                         PERFORM KEYCLR-SUB
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4     処理区分の入力チェック                        *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*
     MOVE    ZERO            TO   ERR-MSG-CD.
*処理区分 CHK
     IF  ( DSP-SYORI  NOT  NUMERIC   )
         MOVE    17     TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
     ELSE
         IF  ( DSP-SYORI  =  1 OR 2 OR 3 )
             IF  DSP-SYORI      =    1
                 MOVE    1           TO   WK-SYORI
             END-IF
             IF  DSP-SYORI      =    2
                 MOVE    2           TO   WK-SYORI
             END-IF
             IF  DSP-SYORI      =    3
                 MOVE    3           TO   WK-SYORI
             END-IF
             MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYORI
             MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYORI
         ELSE
             MOVE   17      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
         END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2       ユーザーＩＤ入力
*--------------------------------------------------------------*
 USERID-SUB                  SECTION.
*
     MOVE    "GRPUSR"   TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           MOVE   "0"          TO   MAIN-FLG
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "F006"
           MOVE   "0"          TO   MAIN-FLG
           MOVE   ZERO         TO   ERR-MSG-CD
           PERFORM  KEYCLR-SUB
           PERFORM  GRPUSRCLR-SUB
           PERFORM  GRPPASCLR-SUB
           PERFORM  BODYCLR-SUB
       WHEN
        "E000"
           PERFORM        USERID-CHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         IF  WK-SYORI  =   3
                             MOVE   "5"   TO   MAIN-FLG
                         ELSE
                             MOVE   "3"   TO   MAIN-FLG
                         END-IF
                         MOVE "D"   TO EDIT-OPTION OF DSP-USERID
                         MOVE SPACE TO EDIT-CURSOR OF DSP-USERID
                         PERFORM GRPUSRCLR-SUB
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 USERID-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    処理モード初期化
*--------------------------------------------------------------*
 HEADDEL1-SUB            SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYORI.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYORI.
*
 HEADDEL1-END.
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
*      2.2.2.1   ファイルの存在チェック                        *
*--------------------------------------------------------------*
 FILCHK-SUB             SECTION.
*
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-USERID      TO   LOG-F01.
     PERFORM JHMLOGF-READ-SUB.
*
* ＩＮＶＡＬＩＤ処理
     IF  JHMLOGF-INV-FLG  =  "INV"
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-USERID
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-USERID
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-USERID
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-USERID
         ELSE
              PERFORM        FILE-SUB
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-USERID
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-USERID
         END-IF
     END-IF.
*
 FILCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイルセット                               *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
*
*↓2019.03.25
     MOVE    LOG-REC          TO   BEFORE-DATA.
*↑2019.03.25
*
*パスワード
     MOVE    LOG-F02          TO   DSP-PASS     OLD-PASS.
*部門ＣＤ
     MOVE    LOG-F03          TO   DSP-BUMON.
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
     MOVE    LOG-F04          TO   DSP-TANCD.
*担当者名取得
     MOVE    PARA-BUMON       TO   TAN-F01.
     MOVE    LOG-F04          TO   TAN-F02.
     PERFORM HTANMS-READ-SUB
     IF      HTANMS-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-TANNM
     ELSE
             MOVE  TAN-F03    TO   DSP-TANNM
     END-IF.
*利用開始日
     MOVE    LOG-F05          TO   DSP-KAISI.
*利用終了日
     MOVE    LOG-F06          TO   DSP-SYURYO.
*初回ＩＤ変更区分
     MOVE    LOG-F07          TO   DSP-CHGKBN   OLD-CHGKBN.
*初回ＩＤ変更日付
     MOVE    LOG-F08          TO   DSP-SYOKID.
*最終パスワード変更日
     MOVE    LOG-F09          TO   DSP-ENDDAT   OLD-ENDDAT.
*保存パスワード１
     MOVE    LOG-F10          TO   DSP-PASS1.
*保存パスワード２
     MOVE    LOG-F11          TO   DSP-PASS2.
*保存パスワード3
     MOVE    LOG-F12          TO   DSP-PASS3.
*ロックアウト回数
     MOVE    LOG-F13          TO   DSP-LOCK.
*ロックアウト日付
     MOVE    LOG-F14          TO   DSP-LCDATE.
*ロックアウト時間
     MOVE    LOG-F15          TO   DSP-JIKAN.
*登録担当者
     MOVE    LOG-F93          TO   DSP-ADDTAN.
*登録担当者名取得
     MOVE    LOG-F92          TO   TAN-F01.
     MOVE    LOG-F93          TO   TAN-F02.
     PERFORM HTANMS-READ-SUB.
     IF      HTANMS-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-ADDTNM
     ELSE
             MOVE  TAN-F03    TO   DSP-ADDTNM
     END-IF.
*登録日付
     MOVE    LOG-F94          TO   DSP-ADDDAT.
*登録時刻
     MOVE    LOG-F95          TO   DSP-ADDTIM.
*更新担当者
     MOVE    LOG-F97          TO   DSP-UPTAN.
*登録担当者名取得
     MOVE    LOG-F96          TO   TAN-F01.
     MOVE    LOG-F97          TO   TAN-F02.
     PERFORM HTANMS-READ-SUB.
     IF      HTANMS-INV-FLG = "INV"
             MOVE  ALL NC"＊" TO   DSP-UPTNM
     ELSE
             MOVE  TAN-F03    TO   DSP-UPTNM
     END-IF.
*更新日付
     MOVE    LOG-F98          TO   DSP-UPDAT.
*更新時刻
     MOVE    LOG-F99          TO   DSP-UPTIM.
***  97.03.10 ED  ***
     IF   WK-SYORI  =   2
          MOVE  "2"          TO   MAIN-FLG
     ELSE
          MOVE  "3"          TO   MAIN-FLG
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
     MOVE   "GRPPAS"         TO   DSP-GROUP.
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
        WHEN  "F006"
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
                  PERFORM  KEYCLR-SUB
                  PERFORM  GRPUSRCLR-SUB
                  PERFORM  GRPPASCLR-SUB
                  PERFORM  BODYCLR-SUB
        WHEN  "E000"
                  PERFORM        PASSCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "4"     TO   MAIN-FLG
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-PASS
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-PASS
                       PERFORM BODYCLR-SUB
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 PASSWD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     パスワードチェック　　　　　                  *
*--------------------------------------------------------------*
 PASSCHK-SUB            SECTION.
*パスワード入力
     IF  DSP-PASS  =  SPACE
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS
         GO             TO   PASSCHK-END
     END-IF.
*６桁以上かチェック
     IF  DSP-PASS(1:1) NOT = SPACE
     AND DSP-PASS(2:1) NOT = SPACE
     AND DSP-PASS(3:1) NOT = SPACE
     AND DSP-PASS(4:1) NOT = SPACE
     AND DSP-PASS(5:1) NOT = SPACE
     AND DSP-PASS(6:1) NOT = SPACE
*2013/01/22 NAV ST ６桁以上→８桁
     AND DSP-PASS(7:1) NOT = SPACE
     AND DSP-PASS(8:1) NOT = SPACE
*2013/01/22 NAV ED ６桁以上→８桁
         CONTINUE
     ELSE
         MOVE   06      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS
         GO             TO   PASSCHK-END
     END-IF.
*数字文字混在チェック
     MOVE    SPACE      TO   WK-PASS  WK-MOJI  WK-SUUJI.
     MOVE    DSP-PASS   TO   WK-PASS.
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
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS
         GO             TO   PASSCHK-END
     END-IF.
*修正の時、過去３回パスワードと同一でないかチェック
     IF  DSP-SYORI   =  "2"
     AND DSP-PASS    NOT =  OLD-PASS
         IF  DSP-PASS1  =  DSP-PASS
         OR  DSP-PASS2  =  DSP-PASS
         OR  DSP-PASS3  =  DSP-PASS
             MOVE   08      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-PASS
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-PASS
         END-IF
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-PASS
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-PASS
     END-IF.
*
 PASSCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力                               *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
*
*****IF     DSP-SYORI = "1"
*           MOVE   "BODY01"         TO   DSP-GROUP
*    ELSE
*           MOVE   "BODY02"         TO   DSP-GROUP
*****END-IF.
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
        WHEN  "F006"
                  MOVE   "3"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
                  PERFORM  KEYCLR-SUB
                  PERFORM  GRPUSRCLR-SUB
                  PERFORM  GRPPASCLR-SUB
                  PERFORM  BODYCLR-SUB
        WHEN  "E000"
                  PERFORM        BODYCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "5"     TO   MAIN-FLG
                       PERFORM  BODYCLR-SUB
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ部チェック　　　　　                  *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*部門ＣＤ入力チェック
     IF  DSP-BUMON  =  SPACE
         MOVE   09      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-BUMON
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-BUMON
     ELSE
*        部門名称取得
         MOVE    "22"             TO   JYO-F01
         MOVE   DSP-BUMON         TO   JYO-F02
         PERFORM HJYOKEN-READ-SUB
         IF      HJYOKEN-INV-FLG = "INV"
                 MOVE  ALL NC"＊" TO   DSP-BUMNM
                 MOVE   09      TO   ERR-MSG-CD
                 MOVE   "R"     TO   EDIT-OPTION  OF  DSP-BUMON
                 MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-BUMON
         ELSE
                 MOVE  JYO-F03  TO   DSP-BUMNM
                 MOVE   "D"     TO   EDIT-OPTION  OF  DSP-BUMON
                 MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-BUMON
         END-IF
     END-IF.
*担当者ＣＤチェック
     IF      DSP-TANCD  =  SPACE
         IF  ERR-MSG-CD = ZERO
             MOVE   10      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TANCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TANCD
     ELSE
*        担当者名称取得
         MOVE   DSP-BUMON         TO   TAN-F01
         MOVE   DSP-TANCD         TO   TAN-F02
         PERFORM HTANMS-READ-SUB
         IF      HTANMS-INV-FLG = "INV"
                 MOVE  ALL NC"＊" TO   DSP-TANNM
                 IF  ERR-MSG-CD = ZERO
                     MOVE   10      TO   ERR-MSG-CD
                 END-IF
                 MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TANCD
                 MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TANCD
         ELSE
                 MOVE  TAN-F03  TO   DSP-TANNM
                 MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TANCD
                 MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TANCD
         END-IF
     END-IF.
*利用開始日チェック
     IF  DSP-KAISI  NOT  NUMERIC
     OR  DSP-KAISI  = ZERO
         IF  ERR-MSG-CD = ZERO
             MOVE   11      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-KAISI
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-KAISI
         MOVE   ZERO    TO   DSP-KAISI
     ELSE
***      利用開始日論理チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-KAISI      TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              IF  ERR-MSG-CD = ZERO
                  MOVE  12       TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KAISI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KAISI
         ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KAISI
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-KAISI
         END-IF
     END-IF.
*利用終了日が入力された場合
     IF  DSP-SYURYO  NOT  NUMERIC
         MOVE       ZERO     TO   DSP-SYURYO
     END-IF.
*
     IF  DSP-SYURYO  NOT =  ZERO
***      利用開始日論理チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-SYURYO     TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              IF  ERR-MSG-CD = ZERO
                  MOVE  12       TO   ERR-MSG-CD
              END-IF
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYURYO
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYURYO
         ELSE
**************利用開始日と終了日の大小チェック
              IF    DSP-KAISI > DSP-SYURYO
                    IF  ERR-MSG-CD = ZERO
                        MOVE  13       TO   ERR-MSG-CD
                    END-IF
                    MOVE  "R"    TO   EDIT-OPTION  OF  DSP-KAISI
                    MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SYURYO
                    MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-KAISI
              ELSE
                    MOVE  "M"    TO   EDIT-OPTION  OF  DSP-KAISI
                    MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SYURYO
                    MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-KAISI
                    MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SYURYO
              END-IF
         END-IF
     END-IF.
*登録の時は以下のチェックは行なわない
     IF  DSP-SYORI  = "1"
         GO                      TO   BODYCHK-END
     END-IF.
*初期ID変更区分
     IF  DSP-CHGKBN  =  SPACE  OR  "1"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHGKBN
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHGKBN
     ELSE
         IF  ERR-MSG-CD = ZERO
             MOVE   14      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CHGKBN
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-CHGKBN
     END-IF.
*最終PASS変更日
     IF  DSP-ENDDAT  NOT NUMERIC
*****OR  DSP-ENDDAT  =   ZERO
         IF  ERR-MSG-CD  = ZERO
             MOVE   15      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-ENDDAT
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-ENDDAT
     ELSE
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-CHGKBN
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-CHGKBN
     END-IF.
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF  ERR-MSG-CD  =  ZERO
         MOVE     "Y"       TO   DSP-KAKUNN
     END-IF.
     MOVE    "GPKAKU"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              MOVE   "0"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KAKUNN
        WHEN  "F005"
              MOVE   "END"        TO   END-FLG
        WHEN  "F006"
              IF   WK-SYORI  =    3
                   MOVE   "2"     TO   MAIN-FLG
              ELSE
                   MOVE   "4"     TO   MAIN-FLG
              END-IF
              MOVE   SPACE        TO   DSP-KAKUNN
              PERFORM  KEYCLR-SUB
              PERFORM  GRPUSRCLR-SUB
              PERFORM  GRPPASCLR-SUB
              PERFORM  BODYCLR-SUB
        WHEN  "E000"
              IF  DSP-KAKUNN  NOT  =  "Y"
                  MOVE   16      TO   ERR-MSG-CD
              ELSE
                  PERFORM  KOUSIN-SUB
                  MOVE   "0"     TO   MAIN-FLG
              END-IF
        WHEN  OTHER
              MOVE   01      TO   ERR-MSG-CD
     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        ファイル更新                                 *
*--------------------------------------------------------------*
 KOUSIN-SUB             SECTION.
*
     EVALUATE  DSP-SYORI
         WHEN  "1"    PERFORM FILE-ADD-SUB
         WHEN  "2"    PERFORM FILE-UPD-SUB
         WHEN  "3"    PERFORM FILE-DEL-SUB
     END-EVALUATE.
*
 KOUSIN-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        登録
*--------------------------------------------------------------*
 FILE-ADD-SUB           SECTION.
*ファイル初期化
     MOVE      SPACE          TO       LOG-REC.
     INITIALIZE                        LOG-REC.
*ユーザーＩＤ
     MOVE      DSP-USERID     TO       LOG-F01.
*パスワード（登録時パスワード１に初回のパスワードを登録）
     MOVE      DSP-PASS       TO       LOG-F02   LOG-F10.
*部門ＣＤ
     MOVE      DSP-BUMON      TO       LOG-F03.
*担当者ＣＤ
     MOVE      DSP-TANCD      TO       LOG-F04.
*利用開始日
     MOVE      DSP-KAISI      TO       LOG-F05.
*利用終了日
     MOVE      DSP-SYURYO     TO       LOG-F06.
*最終PASS変更日
     IF        DSP-ENDDAT NOT NUMERIC
               MOVE ZERO       TO      LOG-F09
     ELSE
               MOVE DSP-ENDDAT TO      LOG-F09
     END-IF.
*登録担当者部門ＣＤ
     MOVE      PARA-BUMON     TO       LOG-F92.
*登録担当者ＣＤ
     MOVE      PARA-TANCD     TO       LOG-F93.
*登録日付
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
     MOVE      LINK-OUT-YMD       TO   LOG-F94.
*登録時刻
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   LOG-F95.
*マスタ履歴ファイル出力
     MOVE      SPACE              TO   MSL-REC.
     INITIALIZE                        MSL-REC.
*マスタ区分
     MOVE      "09"               TO   MSL-F01.
*部門ＣＤ
     MOVE      PARA-BUMON         TO   MSL-F02.
*担当者ＣＤ
     MOVE      PARA-TANCD         TO   MSL-F03.
*更新区分
     MOVE      "1"                TO   MSL-F04.
*更新日付
     MOVE      PARA-SDATE         TO   MSL-F05.
*更新時刻
     MOVE      PARA-STIME         TO   MSL-F06.
*連番
     ADD       1                  TO   WK-SEQ.
     MOVE      WK-SEQ             TO   MSL-F07.
*マスタレコードエリア
     MOVE      LOG-REC            TO   MSL-F08.
*ファイル出力
     WRITE     MSL-REC.
*ファイル出力
     WRITE     LOG-REC.
*件数カウント
     ADD       1              TO       ADD-CNT.
*
 FILE-ADD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.5        修正
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
     MOVE      DSP-PASS       TO       LOG-F02.
*部門ＣＤ
     MOVE      DSP-BUMON      TO       LOG-F03.
*担当者ＣＤ
     MOVE      DSP-TANCD      TO       LOG-F04.
*利用開始日
     MOVE      DSP-KAISI      TO       LOG-F05.
*利用終了日
     MOVE      DSP-SYURYO     TO       LOG-F06.
*初期ＩＤ変更区分
     MOVE      DSP-CHGKBN     TO       LOG-F07.
*初期ＩＤ変更日付
     MOVE      DSP-SYOKID     TO       LOG-F08.
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
     IF        DSP-PASS  NOT =  OLD-PASS
******最終PASS変更日
               MOVE LOG-F98    TO      LOG-F09
******保存パスワード１
               MOVE DSP-PASS  TO       LOG-F10
******保存パスワード２
               MOVE DSP-PASS1 TO       LOG-F11
******保存パスワード３
               MOVE DSP-PASS2 TO       LOG-F12
     ELSE
               IF   DSP-ENDDAT  NOT =  OLD-ENDDAT
                    MOVE DSP-ENDDAT    TO    LOG-F09
               END-IF
     END-IF.
*ロックアウト回数
     MOVE      DSP-LOCK       TO       LOG-F13.
*ロックアウト日付
     MOVE      DSP-LCDATE     TO       LOG-F14.
*ロックアウト時間
     MOVE      DSP-JIKAN      TO       LOG-F15.
*更新担当者部門ＣＤ
     MOVE      PARA-BUMON     TO       LOG-F96.
*更新担当者ＣＤ
     MOVE      PARA-TANCD     TO       LOG-F97.
*更新時刻
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   LOG-F99.
*マスタ履歴ファイル出力
     MOVE      SPACE              TO   MSL-REC.
     INITIALIZE                        MSL-REC.
*マスタ区分
     MOVE      "09"               TO   MSL-F01.
*部門ＣＤ
     MOVE      PARA-BUMON         TO   MSL-F02.
*担当者ＣＤ
     MOVE      PARA-TANCD         TO   MSL-F03.
*更新区分
     MOVE      "2"                TO   MSL-F04.
*更新日付
     MOVE      PARA-SDATE         TO   MSL-F05.
*更新時刻
     MOVE      PARA-STIME         TO   MSL-F06.
*連番
     ADD       1                  TO   WK-SEQ.
     MOVE      WK-SEQ             TO   MSL-F07.
*マスタレコードエリア
     MOVE      LOG-REC            TO   MSL-F08.
*
*↓2019.03.25
*修正前レコードエリア
     PERFORM   BEFORE-SEC.
*↑2019.03.25
*
*ファイル出力
     WRITE     MSL-REC.
*ファイル出力
     REWRITE   LOG-REC.
*件数カウント
     ADD       1              TO       UPD-CNT.
*
 FILE-UPD-END.
     EXIT.
****************************************************************
*  2019.03.25    修正前レコードセット
****************************************************************
 BEFORE-SEC                  SECTION.
*
     IF       DSP-SYORI   =    2
              MOVE  BEFORE-DATA   TO   MSL-F09
     END-IF.
*
 BEFORE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.5        削除
*--------------------------------------------------------------*
 FILE-DEL-SUB           SECTION.
*ファイル索引
     MOVE    DSP-USERID      TO   LOG-F01.
     PERFORM JHMLOGF-READ-SUB.
* ＩＮＶＡＬＩＤ処理
     IF  JHMLOGF-INV-FLG  =  "INV"
         DISPLAY NC"＃削除エラーです。＃"  UPON CONS
         DISPLAY NC"＃削除対象がありま＃"  UPON CONS
         DISPLAY NC"＃せん。確認して下＃"  UPON CONS
         DISPLAY NC"＃さい　　　　　　＃"  UPON CONS
         STOP  RUN
     END-IF.
*マスタ履歴ファイル出力
     MOVE      SPACE              TO   MSL-REC.
     INITIALIZE                        MSL-REC.
*マスタ区分
     MOVE      "09"               TO   MSL-F01.
*部門ＣＤ
     MOVE      PARA-BUMON         TO   MSL-F02.
*担当者ＣＤ
     MOVE      PARA-TANCD         TO   MSL-F03.
*更新区分
     MOVE      "3"                TO   MSL-F04.
*更新日付
     MOVE      PARA-SDATE         TO   MSL-F05.
*更新時刻
     MOVE      PARA-STIME         TO   MSL-F06.
*連番
     ADD       1                  TO   WK-SEQ.
     MOVE      WK-SEQ             TO   MSL-F07.
*マスタレコードエリア
     MOVE      LOG-REC            TO   MSL-F08.
*ファイル出力
     WRITE     MSL-REC.
*ファイル削除
     DELETE    JHMLOGF.
*件数カウント
     ADD       1              TO       DEL-CNT.
*
 FILE-DLT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    DSPF  JHMLOGF  HTANMS  HJYOKEN  MSTLOGF.
*
     DISPLAY "LOGMST ADD CNT = " ADD-CNT  UPON CONS.
     DISPLAY "LOGMST UPD CNT = " UPD-CNT  UPON CONS.
     DISPLAY "LOGMST DEL CNT = " DEL-CNT  UPON CONS.
*
 END-END.
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
*    処理モード初期化
*--------------------------------------------------------------*
 KEYDEL-SUB              SECTION.
*
     MOVE  SPACE        TO   DSP-GRP001.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYORI.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYORI.
*
 KEYDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ユーザーＩＤ初期化
*--------------------------------------------------------------*
 GRPUSRDEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP002.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-USERID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-USERID.
*
 GRPUSRDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    パスワード初期化
*--------------------------------------------------------------*
 GRPPASDEL-SUB           SECTION.
*
     MOVE  SPACE        TO   DSP-GRP003.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS.
*
 GRPPASDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ初期化
*--------------------------------------------------------------*
 BODYDEL-SUB             SECTION.
*
     MOVE  SPACE        TO   DSP-GRP004.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-TANCD.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-KAISI.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYURYO.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-CHGKBN.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYOKID.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-ENDDAT.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS1.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS2.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS3.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-TANCD.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-KAISI.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYURYO.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-CHGKBN.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYOKID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-ENDDAT.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS1.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS2.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS3.
*
 GRPPASDEL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    （共通）画面表示処理
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
*
     MOVE    "PIT00100"      TO   DSP-FORMID.
     MOVE    "SIT0010I"      TO   DSP-PGID.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     WRITE    DSP-FIT00101.
*
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*     （共通）画面読み込み
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
*ＰＦガイドセット
     EVALUATE      MAIN-FLG
         WHEN      "0"       MOVE  PMSG01   TO   DSP-MSG2
         WHEN      "1"       MOVE  PMSG01   TO   DSP-MSG2
         WHEN      "2"       MOVE  PMSG02   TO   DSP-MSG2
         WHEN      "3"       MOVE  PMSG02   TO   DSP-MSG2
         WHEN      "4"       MOVE  PMSG02   TO   DSP-MSG2
         WHEN      "5"       MOVE  PMSG02   TO   DSP-MSG2
         WHEN      OTHER     MOVE  PMSG02   TO   DSP-MSG2
     END-EVALUATE.
*エラーメッセージ表示
     IF       ERR-MSG-CD   =    0
              MOVE      SPACE    TO   DSP-MSG1
              MOVE      "D"      TO   EDIT-OPTION OF DSP-MSG1
     ELSE
              MOVE      ERR-MSG (ERR-MSG-CD)   TO   DSP-MSG1
     END-IF.
*画面ＷＲＩＴＥ
     PERFORM DSP-WRITE-SUB.
*
     MOVE   SPACE       TO   DSP-MSG1.
*
     MOVE  "NE"         TO   DSP-PROC.
*ＰＦガイドセット
     EVALUATE      MAIN-FLG
         WHEN      "1"  MOVE  "KEY"    TO   DSP-GROUP
         WHEN      "2"  MOVE  "GRPUSR" TO   DSP-GROUP
         WHEN      "3"  MOVE  "GRPPAS" TO   DSP-GROUP
         WHEN      "4"  IF     WK-SYORI  =  1
                               MOVE  "BODY01" TO   DSP-GROUP
                        ELSE
                               MOVE  "BODY02" TO   DSP-GROUP
                        END-IF
         WHEN      "5"  MOVE  "GPKAKU" TO   DSP-GROUP
     END-EVALUATE.
     MOVE   "FIT00101"                 TO   DSP-FORMAT.
     READ   DSPF.
*
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*    処理モード初期化
*--------------------------------------------------------------*
 KEYCLR-SUB              SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYORI.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYORI.
*
 KEYCLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ユーザーＩＤ初期化
*--------------------------------------------------------------*
 GRPUSRCLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-USERID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-USERID.
*
 GRPUSRCLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    パスワード初期化
*--------------------------------------------------------------*
 GRPPASCLR-SUB           SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS.
*
 GRPPASCLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ初期化
*--------------------------------------------------------------*
 BODYCLR-SUB             SECTION.
*
     MOVE  " "          TO   EDIT-CURSOR OF DSP-TANCD.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-KAISI.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYURYO.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-CHGKBN.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-SYOKID.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-ENDDAT.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS1.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS2.
     MOVE  " "          TO   EDIT-CURSOR OF DSP-PASS3.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-TANCD.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-KAISI.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYURYO.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-CHGKBN.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-SYOKID.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-ENDDAT.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS1.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS2.
     MOVE  "M"          TO   EDIT-OPTION OF DSP-PASS3.
*
 BODYCLR-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
