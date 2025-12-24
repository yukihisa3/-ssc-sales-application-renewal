# SHS0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SHS0010I.COB`

## ソースコード

```cobol
****************************************************************
*    ユーザ　　　　名：　（株）サカタのタネ殿                  *
*    システム　　　名：　ＤＣＭＪＡＰＡＮ発注種別区分変換　    *
*    プログラム　　名：　発注種別変換マスタ保守　　　　　　　  *
*    作成者／更新者　：　ＮＡＶ高橋　　　                      *
*    作成日／更新日　：　2019/03/11                            *
*    処理概要　　　　：　発注種別変換Ｍの登録修正削除を行なう。*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHS0010I.
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
*発注種別変換マスタ
     SELECT     DCMHSBF   ASSIGN    TO        DA-01-VI-DCMHSBL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       HSB-F01
                                               HSB-F02
                           FILE      STATUS    HSB-STATUS.
*取引先マスタ
     SELECT     HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FHS00101  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  発注種別変換マスタ  >>---*
 FD  DCMHSBF.
     COPY     DCMHSBF  OF        XFDLIB
              JOINING   HSB       PREFIX.
*---<<  取引先マスタ　　　  >>---*
 FD  HTOKMS.
     COPY     HTOKMS   OF        XFDLIB
              JOINING   TOK       PREFIX.
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
     02  HSB-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  DCMHSBF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
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
                           NC"_取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_再入力".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHS0010I".
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
            NC"対象データが存在しません。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"取引先マスタ未登録です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"必須項目です！！".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"処理区分は１，２，３です。".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"発注種別区分は２桁で入力して下さい。".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"　　　　　　　　　　　　".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  9   TIMES.
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
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
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
                     PROCEDURE    DCMHSBF.
     MOVE     "DCMHSBL1"     TO   ERR-FL-ID.
     MOVE     HSB-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTOKMS.
     MOVE     "TOKMS2  "     TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 SHS0010I-START              SECTION.
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
*
 SHS0010I-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*
     OPEN    I-O             DSPF.
     OPEN    I-O             DCMHSBF.
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
     MOVE    "FHS00101"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FHS00101.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    2               TO   WK-SYORI.
     MOVE    SPACE           TO   DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
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
*      2.1       処理区分入力                                  *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
*
     PERFORM       MSG-SEC.
     MOVE     PMSG03    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "SYORI"    TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F005"
           MOVE   "END"        TO   END-FLG
       WHEN
        "E000"
           PERFORM        SYORICHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         MOVE   "2"   TO   MAIN-FLG
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-SYORI
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-SYORI
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.1     画面表示処理                                  *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE    "GPALL"         TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     WRITE    DSP-FHS00101.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.2     エラーメッセージセット                        *
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
*      2.1.3     画面データの入力処理                          *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"         TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*      2.1.4     処理区分の入力チェック                        *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*
     PERFORM GAMEN-CLR-SEC.
*処理区分 CHK
     IF  ( DSP-SYORI  NOT  NUMERIC   )
         MOVE   07      TO   ERR-MSG-CD
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
             MOVE   07      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYORI
             MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYORI
         END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2       画面クリア処理                                *
*--------------------------------------------------------------*
 GAMEN-CLR-SEC               SECTION.
*
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYORI.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYORI.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HACSYB.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HACSYB.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HENHAK.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HENHAK.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HACNAM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HACNAM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HACKAN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HACKAN.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KOGNAM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KOGNAM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-NOUNAM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-NOUNAM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CENNAM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CENNAM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYUNAM.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYUNAM.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
*
 GAMEN-CLR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*      2.2       ＨＥＡＤ部入力                                *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
*
     PERFORM       MSG-SEC.
     MOVE     PMSG01    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "GPHEAD"   TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE    DSP-FUNC
       WHEN
        "F004"
           PERFORM        HEADDEL-SUB
           PERFORM        BODYDEL-SUB
           PERFORM        GAMEN-CLR-SEC
           MOVE   "1"          TO   MAIN-FLG
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "E000"
           PERFORM        HEADCHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         IF  WK-SYORI  =   3
                             MOVE   "4"   TO   MAIN-FLG
                         ELSE
                             MOVE   "3"   TO   MAIN-FLG
                         END-IF
                  END-IF
       WHEN
         OTHER
           MOVE   01     TO   ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.1     ＨＥＡＤ部消去                                *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-HEAD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*
     PERFORM   GAMEN-CLR-SEC.
*
     IF  DSP-TOKCD  NOT  NUMERIC
         MOVE   ZERO    TO   DSP-TOKCD
     END-IF.
*取引先マスタ読込
     MOVE  DSP-TOKCD    TO   TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = "INV"
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
         GO             TO   HEADCHK-END
     ELSE
         MOVE  TOK-F02  TO   DSP-TOKNM
     END-IF.
*発注種別区分
     IF  DSP-HACSYB       =    SPACE
     OR  DSP-HACSYB(1:1)  =    SPACE
     OR  DSP-HACSYB(2:1)  =    SPACE
         MOVE   08      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
         GO             TO   HEADCHK-END
     ELSE
         PERFORM   FILCHK-SUB
     END-IF.
*
 HEADCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   ファイルの存在チェック                        *
*--------------------------------------------------------------*
 FILCHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-TOKCD       TO   HSB-F01.
     MOVE    DSP-HACSYB      TO   HSB-F02.
     READ    DCMHSBF
         INVALID   KEY
             MOVE  "INV"     TO   DCMHSBF-INV-FLG
         NOT INVALID KEY
             MOVE  SPACE     TO   DCMHSBF-INV-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  DCMHSBF-INV-FLG  =  "INV"
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-HACSYB
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-HACSYB
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-TOKCD
         ELSE
              PERFORM        FILE-SUB
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
     MOVE    HSB-F01          TO   DSP-TOKCD.
     MOVE    HSB-F02          TO   DSP-HACSYB.
     MOVE    HSB-F03          TO   DSP-HENHAK.
     MOVE    HSB-F04          TO   DSP-HACNAM.
     MOVE    HSB-F05          TO   DSP-HACKAN.
     MOVE    HSB-F06          TO   DSP-KOGNAM.
     MOVE    HSB-F07          TO   DSP-NOUNAM.
     MOVE    HSB-F08          TO   DSP-CENNAM.
     MOVE    HSB-F09          TO   DSP-SYUNAM.
*
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
 BODY-SUB          SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02    TO   DSP-MSG2.
     PERFORM       DSP-WRITE-SUB.
     MOVE   "GPBODY"         TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE    ZERO            TO   ERR-MSG-CD.
* アテンション判定
     EVALUATE      DSP-FUNC
        WHEN  "F004"
                  PERFORM        HEADDEL-SUB
                  PERFORM        BODYDEL-SUB
                  PERFORM        GAMEN-CLR-SEC
                  MOVE   "1"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F009"
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "E000"
                  PERFORM        BODYCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "4"     TO   MAIN-FLG
                  END-IF
        WHEN  OTHER
                  MOVE    01          TO   ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.1     ＢＯＤＹ部消去                                *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   DSP-BODY.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ入力チェック                          *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     PERFORM   GAMEN-CLR-SEC.
*発注種別変換区分
     IF  DSP-HENHAK     =  SPACE
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-HENHAK
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-HENHAK
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HENHAK
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HENHAK
     END-IF.
*発注種別区分名
     IF  DSP-HACNAM     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HACNAM
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HACNAM
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-HACNAM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-HACNAM
     END-IF.
*発注種別区分名カナ
     IF  DSP-HACKAN     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-HACKAN
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-HACKAN
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-HACKAN
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-HACKAN
     END-IF.
*個口記入一覧名
     IF  DSP-KOGNAM     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KOGNAM
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KOGNAM
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-KOGNAM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-KOGNAM
     END-IF.
*納品ラベル名
     IF  DSP-NOUNAM     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-NOUNAM
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-NOUNAM
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-NOUNAM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-NOUNAM
     END-IF.
*センター納入情報
     IF  DSP-CENNAM     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CENNAM
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CENNAM
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CENNAM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-CENNAM
     END-IF.
*出荷リスト名
     IF  DSP-SYUNAM     NOT =  SPACE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SYUNAM
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SYUNAM
     ELSE
         IF  ERR-MSG-CD  =  ZERO
             MOVE 05    TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SYUNAM
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SYUNAM
     END-IF.
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.4       確認入力                                      *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF  ERR-MSG-CD  =  ZERO
         MOVE     "Y"       TO   DSP-KAKNIN
     END-IF.
     PERFORM       MSG-SEC.
     IF  WK-SYORI  =  3
         MOVE     PMSG01    TO   DSP-MSG2
     ELSE
         MOVE     PMSG02    TO   DSP-MSG2
     END-IF.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "GPKAKU"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
     MOVE     ZERO           TO    ERR-MSG-CD.
 KAKUNIN.
* アテンション判定
     EVALUATE  DSP-FUNC
        WHEN  "F004"
              PERFORM        HEADDEL-SUB
              PERFORM        BODYDEL-SUB
              PERFORM        GAMEN-CLR-SEC
              MOVE   "1"          TO   MAIN-FLG
              MOVE   ZERO         TO   ERR-MSG-CD
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "F009"
              IF   WK-SYORI  =    3
                   MOVE   01      TO   ERR-MSG-CD
              ELSE
                   MOVE   "3"     TO   MAIN-FLG
              END-IF
              MOVE   SPACE        TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   06      TO   ERR-MSG-CD
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
*登録の場合
     IF      WK-SYORI   =    1
             MOVE   SPACE         TO   HSB-REC
             INITIALIZE      HSB-REC
     END-IF.
     MOVE    DSP-TOKCD            TO   HSB-F01.
     MOVE    DSP-HACSYB           TO   HSB-F02.
     MOVE    DSP-HENHAK           TO   HSB-F03.
     MOVE    DSP-HACNAM           TO   HSB-F04.
     MOVE    DSP-HACKAN           TO   HSB-F05.
     MOVE    DSP-KOGNAM           TO   HSB-F06.
     MOVE    DSP-NOUNAM           TO   HSB-F07.
     MOVE    DSP-CENNAM           TO   HSB-F08.
     MOVE    DSP-SYUNAM           TO   HSB-F09.
*登録の場合
     IF      WK-SYORI   =   1
             MOVE  PARA-BUMONCD   TO   HSB-F92
             MOVE  PARA-TANCD     TO   HSB-F93
             MOVE   DATE-AREA     TO   HSB-F94
             MOVE   WK-TIME(1:6)  TO   HSB-F95
     END-IF.
*更新日は登録／修正の時に更新する。
     MOVE    PARA-BUMONCD         TO   HSB-F96.
     MOVE    PARA-TANCD           TO   HSB-F97.
     MOVE    DATE-AREA            TO   HSB-F98.
     MOVE    WK-TIME(1:6)         TO   HSB-F99.
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
        WHEN  1
                   WRITE      HSB-REC
                   END-WRITE
        WHEN  2
                   REWRITE    HSB-REC
                   END-REWRITE
        WHEN  3
                   DELETE     DCMHSBF
                   END-DELETE
     END-EVALUATE.
*
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
*****MOVE   "1"         TO   MAIN-FLG.
     MOVE   "2"         TO   MAIN-FLG.
     MOVE   WK-SYORI    TO   DSP-SYORI.
 FILPRT-END.
     EXIT.
****************************************************************
*    取引先マスタ読込　　　　                                  *
****************************************************************
 HTOKMS-READ-SEC        SECTION.
*
     READ   HTOKMS
            INVALID      MOVE  "INV"    TO   HTOKMS-INV-FLG
            NOT  INVALID MOVE  SPACE    TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      DCMHSBF    HTOKMS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
