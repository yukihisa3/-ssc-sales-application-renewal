****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣシステム　　　　　　　　　*
*    モジュール名　　　　：　ＥＤＩＣ名称マスタ保守　　　　　　*
*    作成日／更新日　　　：　15/08/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＥＤＩＣ名称マスタの登録・修正　　*
*                            ・削除を行う。                    *
*    更新日　　　　　　　：　                                  *
*    　　　　　　　　　　：                                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED9002I.
 AUTHOR.                NAV.
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
****************************************************************
 INPUT-OUTPUT           SECTION.
****************************************************************
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
*---<<  ＥＤＩＣ名称マスタ  >>---*
     SELECT   EDMEISF   ASSIGN    TO        DA-01-VI-EDMEISL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                                                 MEI-F02
                                                 MEI-F04
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  取引先マスタ　　　  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FED90021  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  ＥＤＩＣ名称マスタ  >>---*
 FD  EDMEISF.
     COPY     EDMEISF   OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
****************************************************************
 WORKING-STORAGE             SECTION.
****************************************************************
****  作業領域  ***
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  EDMEISF-INV-FLG     PIC  X(03)  VALUE ZERO.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE ZERO.
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
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
                           NC"４：取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"４：取消　６：項目戻り".
     02  PMSG03            PIC N(20) VALUE
                           NC"５：終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SED9002I".
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
            NC"処理区分を入力して下さい。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"処理区分＝１，２，３以外は指定出来ません。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"取引先コードが入力して下さい。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"取引先マスタ未登録です。".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"ＥＤＩＣ名称区分コードを入力して下さい。".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"ＥＤＩＣ名称コードを入力して下さい。".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"代表名称情報が未登録です。".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"ＥＤＩＣ名称マスタ未登録です。".
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"ＥＤＩＣ名称マスタに登録済です。".
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"ＥＤＩＣ名称名（漢字）を入力して下さい。".
     02  MSG-ERR12           PIC  N(28) VALUE
            NC"ＥＤＩＣ名称区分名（カナ）を入力して下さい。".
     02  MSG-ERR13           PIC  N(28) VALUE
            NC"確認＝”Ｙ”を指定して下さい。".
     02  MSG-ERR14           PIC  N(28) VALUE
            NC"ＥＤＩＣ名称区分名を入力して下さい。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  14  TIMES.
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
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(02).
***************************************************************
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
***************************************************************
 PROCEDURE                   DIVISION  USING PARA-BUMONCD
                                             PARA-TANCD.
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
                     PROCEDURE    EDMEISF.
     MOVE     "EDMEISL1"     TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE     "TOKMS1  "     TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 CONTROL-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
****************************************************************
*      1.0     初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             DSPF.
     OPEN    I-O             EDMEISF.
     OPEN    INPUT           HTOKMS.
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
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "FED90021"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FED90021.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    2               TO   WK-SYORI.
     MOVE    SPACE           TO   DSP-PROC.
*
 INIT-END.
     EXIT.
****************************************************************
*      2.0     メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"       PERFORM   SYORI-SUB
         WHEN      "2"       PERFORM   HEAD-SUB
         WHEN      "3"       PERFORM   BODY-SUB
         WHEN      "4"       PERFORM   KAKUNIN-SUB
         WHEN      "5"       PERFORM   FILPRT-SUB
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       画面初期化処理                                *
*--------------------------------------------------------------*
 CLR-SUB                     SECTION.
*属性初期化
     PERFORM       KEY001-CLR-SUB.
     PERFORM       KEY002-CLR-SUB.
     PERFORM       GRP001-CLR-SUB.
*項目初期化
     PERFORM       MODEDEL-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
 CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.1       処理区分入力                                  *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
*属性初期化
*    PERFORM       KEY001-CLR-SUB.
*    PERFORM       KEY002-CLR-SUB.
*    PERFORM       GRP001-CLR-SUB.
*項目初期化
*    PERFORM       MODEDEL-SUB.
*    PERFORM       HEADDEL-SUB.
*    PERFORM       BODYDEL-SUB.
*
     PERFORM       DSP-WRITE-SUB.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
           PERFORM  KEY001-CLR-SUB
           PERFORM        SYORICHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         MOVE   "2"   TO   MAIN-FLG
                         PERFORM  KEY001-CLR-SUB
                         PERFORM  HEADDEL-SUB
                         PERFORM  BODYDEL-SUB
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
*処理区分 CHK
     IF  ( DSP-SYORI  NOT  NUMERIC   )
         MOVE   02      TO   ERR-MSG-CD
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
         ELSE
             MOVE   03      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
         END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2       ＨＥＡＤ部入力                                *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
*属性初期化
*    PERFORM       KEY002-CLR-SUB.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           MOVE   "1"          TO   MAIN-FLG
           PERFORM  CLR-SUB
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "F006"
           MOVE   "1"          TO   MAIN-FLG
           PERFORM  KEY002-CLR-SUB
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "E000"
           PERFORM  KEY002-CLR-SUB
           PERFORM        HEADCHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         IF  WK-SYORI  =   3
                             MOVE   "4"   TO   MAIN-FLG
                         ELSE
                             MOVE   "3"   TO   MAIN-FLG
                         END-IF
                         PERFORM  KEY002-CLR-SUB
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*取引先ＣＤチェック
     IF  DSP-TOKCD  NOT NUMERIC
         MOVE   04          TO   ERR-MSG-CD
         MOVE   "R"         TO   EDIT-OPTION  OF  DSP-TOKCD
         MOVE   "C"         TO   EDIT-CURSOR  OF  DSP-TOKCD
     ELSE
         IF  DSP-TOKCD NOT = ZERO
           MOVE DSP-TOKCD TO   TOK-F01
           PERFORM  HTOKMS-READ-SUB
           IF  HTOKMS-INV-FLG  =  "INV"
               MOVE   05      TO   ERR-MSG-CD
               MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
               MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
           ELSE
               MOVE TOK-F02   TO   DSP-TOKNM
           END-IF
         ELSE
           MOVE NC"代表名称登録"
                              TO   DSP-TOKNM
         END-IF
     END-IF.
*ＥＤＩＣ名称区分コード
     IF  DSP-MEIKBN  =  SPACE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   06          TO   ERR-MSG-CD
         END-IF
         MOVE   "R"         TO   EDIT-OPTION  OF  DSP-MEIKBN
         MOVE   "C"         TO   EDIT-CURSOR  OF  DSP-MEIKBN
     END-IF.
*ＥＤＩＣ名称コード
     IF  DSP-MEICD   =  SPACE
         IF  ERR-MSG-CD  =  ZERO
             MOVE   07      TO   ERR-MSG-CD
         END-IF
         MOVE   "R"         TO   EDIT-OPTION  OF  DSP-MEICD
         MOVE   "C"         TO   EDIT-CURSOR  OF  DSP-MEICD
     END-IF.
*エラー判定
     IF  ERR-MSG-CD  NOT =  ZERO
         GO                 TO   HEADCHK-END
     END-IF.
*取引先ＣＤ＝０以外の時、マスタを読込
     IF  DSP-TOKCD  NOT =  ZERO
         MOVE   ZERO        TO   MEI-F01
         MOVE   DSP-MEIKBN  TO   MEI-F02
         MOVE   DSP-MEICD   TO   MEI-F04
         PERFORM  EDMEISF-READ-SUB
         IF  EDMEISF-INV-FLG  =  "INV"
*************ＭＳＧ－代表名称情報が未登録です。
*            MOVE   08      TO   ERR-MSG-CD
*            MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
*            MOVE   "R"     TO   EDIT-OPTION  OF  DSP-MEIKBN
*            MOVE   "R"     TO   EDIT-OPTION  OF  DSP-MEICD
*************MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
             MOVE SPACE     TO   DSP-MEIKBW  DSP-MEIKNM
         ELSE
             MOVE MEI-F03   TO   DSP-MEIKBW  DSP-MEIKNM
         END-IF
     END-IF.
*ＥＤＩＣ名称マスタを読込
     MOVE  DSP-TOKCD        TO   MEI-F01.
     MOVE  DSP-MEIKBN       TO   MEI-F02.
     MOVE  DSP-MEICD        TO   MEI-F04.
     PERFORM  EDMEISF-READ-SUB.
*存在チェック判断
     IF   EDMEISF-INV-FLG = "INV"
**********ファイル無しで修正／削除の場合
**********ＭＳＧ－ＥＤＩＣ名称マスタ未登録です。
          IF   WK-SYORI  NOT =  1
               IF  ERR-MSG-CD  =  ZERO
                   MOVE  09     TO   ERR-MSG-CD
               END-IF
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TOKCD
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-MEIKBN
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-MEICD
               MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TOKCD
          END-IF
          GO                TO   HEADCHK-END
     ELSE
**********ファイル存在で登録の場合
**********ＭＳＧ－ＥＤＩＣ名称マスタに既に登録済です。
          IF   WK-SYORI  =  1
               IF  ERR-MSG-CD  =  ZERO
                   MOVE  10     TO   ERR-MSG-CD
               END-IF
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TOKCD
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-MEIKBN
               MOVE  "R"    TO   EDIT-OPTION  OF  DSP-MEICD
               MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TOKCD
               GO           TO   HEADCHK-END
          END-IF
     END-IF.
*項目セット
     PERFORM  FILE-SUB.
*
 HEADCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイルセット                               *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
*
*ＥＤＩＣ名称区分名
     MOVE    MEI-F03          TO   DSP-MEIKBW.
     MOVE    MEI-F03          TO   DSP-MEIKNM.
*ＥＤＩＣ名称名（漢字）
     MOVE    MEI-F05          TO   DSP-MEIMEI.
*ＥＤＩＣ名称名カナ
     MOVE    MEI-F06          TO   DSP-MEIKAN.
*備考欄
     MOVE    MEI-F07(1:20)    TO   DSP-BIKO1.
     MOVE    MEI-F07(21:20)   TO   DSP-BIKO2.
     MOVE    MEI-F07(41:10)   TO   DSP-BIKO3.
*処理制御
     IF   WK-SYORI  =   2
          MOVE  "3"          TO   MAIN-FLG
     ELSE
          MOVE  "4"          TO   MAIN-FLG
     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力                               *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
*属性初期化
*    PERFORM       GRP001-CLR-SUB.
*****登録モード以外は入力不可とする。
*    IF   WK-SYORI  NOT =  1
**********IF  DSP-TOKCD NOT = ZERO
*             MOVE    "X"    TO   EDIT-STATUS OF DSP-MEIKNM
**********END-IF.
*    END-IF.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  MOVE   "1"          TO   MAIN-FLG
                  PERFORM  CLR-SUB
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F006"
                  MOVE   "2"          TO   MAIN-FLG
                  PERFORM  GRP001-CLR-SUB
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "E000"
                  PERFORM  GRP001-CLR-SUB
                  PERFORM        BODYCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "4"     TO   MAIN-FLG
                       PERFORM  GRP001-CLR-SUB
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ入力チェック                          *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*ＥＤＩＣ名称区分名（漢字）
     IF  DSP-MEIKNM     =  SPACE
         MOVE   14      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-MEIKNM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-MEIKNM
     END-IF.
*ＥＤＩＣ名称区分名（漢字）
     IF  DSP-MEIMEI     =  SPACE
         IF  ERR-MSG-CD  =  ZERO
             MOVE  11   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-MEIMEI
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-MEIMEI
     END-IF.
*ＥＤＩＣ名称区分名（カナ）
     IF  DSP-MEIKAN     =  SPACE
         IF  ERR-MSG-CD  =  ZERO
             MOVE  12   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-MEIKAN
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-MEIKAN
     END-IF.
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE    "SCREEN"             TO   DSP-GROUP.
     MOVE     SPACE               TO   DSP-PROC.
*システム日付取得
     ACCEPT   WK-TIME             FROM TIME.
*画面表示時刻編集
     MOVE     WK-TIME(1:2)        TO   HEN-TIME-HH.
     MOVE     WK-TIME(3:2)        TO   HEN-TIME-MM.
     MOVE     WK-TIME(5:2)        TO   HEN-TIME-SS.
     MOVE     HEN-DATE            TO   DSP-SDATE.
     MOVE     HEN-TIME            TO   DSP-STIME.
*ＰＦガイドセット
     EVALUATE      MAIN-FLG
         WHEN      "1"       MOVE     PMSG03    TO   DSP-MSG2
         WHEN      "2"       MOVE     PMSG02    TO   DSP-MSG2
         WHEN      "3"       MOVE     PMSG02    TO   DSP-MSG2
         WHEN      "4"
                   IF  WK-SYORI  =  3
                       MOVE          PMSG01     TO   DSP-MSG2
                   ELSE
                       MOVE          PMSG02     TO   DSP-MSG2
                   END-IF
     END-EVALUATE.
*
     PERFORM MSG-SEC.
*
     WRITE    DSP-FED90021.
*
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       エラーメッセージセット                        *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
* エラー メッセージ セット
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   DSP-MSG1
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)   TO   DSP-MSG1
         MOVE    ZERO                  TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       画面データの入力処理                          *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
*グループセット
     EVALUATE      MAIN-FLG
         WHEN      "1"       MOVE    "KEY001"   TO   DSP-GROUP
         WHEN      "2"       MOVE    "KEY002"   TO   DSP-GROUP
         WHEN      "3"       MOVE    "GRP001"   TO   DSP-GROUP
         WHEN      "4"       MOVE    "KAKNIN"   TO   DSP-GROUP
     END-EVALUATE.
*
     MOVE  "NE"         TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
*
     IF  ERR-MSG-CD  =  ZERO
         MOVE     "Y"       TO   DSP-KAKNIN
     END-IF.
*属性初期化
     PERFORM       DSP-WRITE-SUB.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              MOVE   "1"          TO   MAIN-FLG
              PERFORM  CLR-SUB
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "F006"
              IF   WK-SYORI  =    3
                   MOVE   01      TO   ERR-MSG-CD
              ELSE
                   MOVE   "3"     TO   MAIN-FLG
              END-IF
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   13      TO   ERR-MSG-CD
              ELSE
                  MOVE   "5"     TO   MAIN-FLG
                  MOVE   SPACE   TO   DSP-KAKNIN
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
 FILPRT-SUB             SECTION.
*登録モードの時、ファイル初期化
 FILPRT-001.
     IF      WK-SYORI   =    1
             MOVE   SPACE         TO   MEI-REC
             INITIALIZE                MEI-REC
     END-IF.
 FILPRT-002.
*取引先ＣＤ
     MOVE    DSP-TOKCD            TO   MEI-F01.
 FILPRT-003.
*ＥＤＩＣ名称区分コード
     MOVE    DSP-MEIKBN           TO   MEI-F02.
     MOVE    DSP-MEIKNM           TO   MEI-F03.
     MOVE    DSP-MEICD            TO   MEI-F04.
     MOVE    DSP-MEIMEI           TO   MEI-F05.
     MOVE    DSP-MEIKAN           TO   MEI-F06.
     MOVE    DSP-BIKO1            TO   MEI-F07(1:20).
     MOVE    DSP-BIKO2            TO   MEI-F07(21:20).
     MOVE    DSP-BIKO3            TO   MEI-F07(41:10).
 FILPRT-004.
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
        WHEN  1
                   MOVE  PARA-BUMONCD  TO  MEI-F93
                   MOVE  PARA-TANCD    TO  MEI-F92
                   ACCEPT WK-TIME   FROM   TIME
                   MOVE  SYS-DATE      TO  MEI-F94
                   MOVE  WK-TIME(1:6)  TO  MEI-F95
                   WRITE MEI-REC
                   END-WRITE
        WHEN  2
                   MOVE  PARA-BUMONCD  TO  MEI-F97
                   MOVE  PARA-TANCD    TO  MEI-F96
                   ACCEPT WK-TIME   FROM   TIME
                   MOVE  SYS-DATE      TO  MEI-F98
                   MOVE  WK-TIME(1:6)  TO  MEI-F99
                   REWRITE    MEI-REC
                   END-REWRITE
        WHEN  3
                   DELETE     EDMEISF
                   END-DELETE
     END-EVALUATE.
 FILPRT-005.
*
*****PERFORM       MODEDEL-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     PERFORM       KEY001-CLR-SUB.
     PERFORM       KEY002-CLR-SUB.
     PERFORM       GRP001-CLR-SUB.
     MOVE   "2"         TO   MAIN-FLG.
*
 FILPRT-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       処理モード消去                                *
*--------------------------------------------------------------*
 MODEDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-SRGRP.
 MODEDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       ＨＥＡＤ部消去                                *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-KYGRP.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       ＢＯＤＹ部消去                                *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-MEGRP.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      ALL       属性初期化（キー１）                          *
*--------------------------------------------------------------*
 KEY001-CLR-SUB              SECTION.
*
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYORI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYORI.
*
 KEY001-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      ALL       属性初期化（キー２）                          *
*--------------------------------------------------------------*
 KEY002-CLR-SUB              SECTION.
*
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-MEIKBN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-MEIKBN.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-MEICD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-MEICD.
*
 KEY002-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      ALL       属性初期化（ボディー）                        *
*--------------------------------------------------------------*
 GRP001-CLR-SUB              SECTION.
*
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-MEIKNM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-MEIKNM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-MEIMEI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-MEIMEI.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-MEIKAN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-MEIKAN.
*
 GRP001-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      ALL       ＥＤＩＣ名称マスタ読込　　                    *
*--------------------------------------------------------------*
 EDMEISF-READ-SUB            SECTION.
*
     READ  EDMEISF
           INVALID       MOVE  "INV"  TO  EDMEISF-INV-FLG
           NOT  INVALID  MOVE  SPACE  TO  EDMEISF-INV-FLG
     END-READ.
*
 EDMEISF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      ALL       取引先マスタ読込　　　　　                    *
*--------------------------------------------------------------*
 HTOKMS-READ-SUB             SECTION.
*
     READ  HTOKMS
           INVALID       MOVE  "INV"  TO  HTOKMS-INV-FLG
           NOT  INVALID  MOVE  SPACE  TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF  EDMEISF  HTOKMS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
