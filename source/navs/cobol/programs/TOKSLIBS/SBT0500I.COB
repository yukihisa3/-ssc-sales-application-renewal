****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷（ＬＩＮＫＳ連携）　　　　　　*
*    モジュール名　　　　：　出荷パターンマスタ保守　　　　　　*
*    作成日／更新日　　　：　2014/07/17                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　マスタ登録・修正・削除            *
*    更新日　　　　　　　：　    /  /                          *
*    　　　　　　　　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0500I.
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
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS1   ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*---<<  得意先マスタ  >>---*
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*---<<  出荷パターンマスタ  >>---*
     SELECT   LNKSPML1  ASSIGN    TO        DA-01-VI-LNKSPML1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SPM-F01
                                                 SPM-F02
                                                 SPM-F03
                                                 SPM-F04
                        FILE      STATUS    IS   SPM-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FBT05001  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS1.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  得意先マスタ  >>---*
 FD  TOKMS2.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  出荷パターンマスタ  >>---*
 FD  LNKSPML1.
     COPY     LNKSPML1  OF        XFDLIB
              JOINING   SPM       PREFIX.
*--------------------------------------------------------------
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
     02  SOK-STATUS          PIC  X(02).
     02  TOK-STATUS          PIC  X(02).
     02  SPM-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBT0500I".
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
            NC"マスタ未登録です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"出荷パターンコードが未入力です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"倉庫名が未入力です".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"倉庫マスタ未登録です".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"取引先マスタ未登録です".
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"ＯＮＬ／手書区分が違います".
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"納品形態が違います".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  11  TIMES.
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
 01  PARA-TANCD            PIC X(08).
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
                     PROCEDURE    ZSOKMS1.
     MOVE     "ZSOKMS1"      TO   ERR-FL-ID.
     MOVE     SOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TOKMS2.
     MOVE     "TOKMS2  "     TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    LNKSPML1.
     MOVE     "LNKSPML1"     TO   ERR-FL-ID.
     MOVE     SPM-STATUS     TO   ERR-STCD.
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
*          　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             DSPF     LNKSPML1.
     OPEN    INPUT           ZSOKMS1  TOKMS2.
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
     ACCEPT    WK-TIME            FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     MOVE    "FBT05001"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FBT05001.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    2               TO   WK-SYORI.
     MOVE    SPACE           TO   DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*          　　メイン処理                                      *
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
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
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
     WRITE    DSP-FBT05001.
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
*処理区分 CHK
     IF  ( DSP-SYORI  NOT  NUMERIC   )
         MOVE   05      TO   ERR-MSG-CD
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
             MOVE   05      TO   ERR-MSG-CD
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
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SPTNCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SPTNCD.
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
           MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKCD
           MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SOKCD
           MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TORCD
           MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-TORCD
           MOVE   "D"     TO   EDIT-OPTION  OF  DSP-OTKBN
           MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-OTKBN
           MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CENKBN
           MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CENKBN
           PERFORM        BODYDEL-SUB
           MOVE   "1"          TO   MAIN-FLG
           MOVE   ZERO         TO   ERR-MSG-CD
       WHEN
        "E000"
           PERFORM       HEADCHK-SUB
                  IF     ERR-MSG-CD   =    ZERO
                         IF  WK-SYORI  =   3
                             MOVE   "4"   TO   MAIN-FLG
                         ELSE
                             MOVE   "3"   TO   MAIN-FLG
                         END-IF
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-SOKCD
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-SOKCD
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-TORCD
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-TORCD
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-OTKBN
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-OTKBN
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-CENKBN
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-CENKBN
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
     MOVE  SPACE        TO   DSP-MAS001.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*
*    IF  DSP-SOKCD      =    SPACE
*        MOVE   04      TO   ERR-MSG-CD
*        MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
*        MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
*    ELSE
*        PERFORM   SOKCHK-SUB
*    END-IF.
*
*   倉庫マスタ存在チェック
     PERFORM   SOKCHK-SUB.
     IF        ERR-MSG-CD    =   ZERO
               CONTINUE
     ELSE
               GO  TO        HEADCHK-END
     END-IF.
*
*   取引先マスタ存在チェック
     PERFORM   TOKCHK-SUB.
     IF        ERR-MSG-CD    =   ZERO
               CONTINUE
     ELSE
               GO  TO          HEADCHK-END
     END-IF.
*
*   ＯＮＬ/手書区分チェック
     IF        DSP-OTKBN     =  "1" OR "2"
               MOVE   ZERO     TO   ERR-MSG-CD
               MOVE   "D"      TO   EDIT-OPTION  OF  DSP-OTKBN
               MOVE   " "      TO   EDIT-CURSOR  OF  DSP-OTKBN
     ELSE
               MOVE   10       TO   ERR-MSG-CD
               MOVE   "R"      TO   EDIT-OPTION  OF  DSP-OTKBN
               MOVE   "C"      TO   EDIT-CURSOR  OF  DSP-OTKBN
               GO  TO          HEADCHK-END
     END-IF.
*
*   納品形態チェック
     IF        DSP-CENKBN      NOT  NUMERIC
               MOVE   00       TO   DSP-CENKBN
     END-IF.
     IF        DSP-CENKBN   =  00   OR  01
               MOVE   ZERO     TO   ERR-MSG-CD
               MOVE   "D"      TO   EDIT-OPTION  OF  DSP-CENKBN
               MOVE   " "      TO   EDIT-CURSOR  OF  DSP-CENKBN
     ELSE
               MOVE   11       TO   ERR-MSG-CD
               MOVE   "R"      TO   EDIT-OPTION  OF  DSP-CENKBN
               MOVE   "C"      TO   EDIT-CURSOR  OF  DSP-CENKBN
               GO  TO          HEADCHK-END
     END-IF.
*
*   出荷パターンマスタ存在チェック
     PERFORM   SPMCHK-SUB.
     IF        ERR-MSG-CD    =   ZERO
               CONTINUE
     ELSE
               GO  TO        HEADCHK-END
     END-IF.
*
 HEADCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   倉庫マスタ存在チェック                        *
*--------------------------------------------------------------*
 SOKCHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-SOKCD       TO   SOK-F01.
     READ    ZSOKMS1
         INVALID   KEY
*            MOVE   1        TO   INVALID-FLG
             MOVE   08       TO   ERR-MSG-CD
             MOVE   "R"      TO   EDIT-OPTION  OF  DSP-SOKCD
             MOVE   "C"      TO   EDIT-CURSOR  OF  DSP-SOKCD
             MOVE   SPACE    TO   DSP-SOKNM
         NOT INVALID KEY
*            MOVE   ZERO     TO   INVALID-FLG
             MOVE   ZERO     TO   ERR-MSG-CD
             MOVE   "D"      TO   EDIT-OPTION  OF  DSP-SOKCD
             MOVE   " "      TO   EDIT-CURSOR  OF  DSP-SOKCD
             MOVE   SOK-F02  TO   DSP-SOKNM
     END-READ.
*
 SOKCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   取引先マスタ存在チェック                      *
*--------------------------------------------------------------*
 TOKCHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-TORCD       TO   TOK-F01.
     READ    TOKMS2
         INVALID   KEY
*            MOVE  1         TO   INVALID-FLG
             MOVE   09       TO   ERR-MSG-CD
             MOVE   "R"      TO   EDIT-OPTION  OF  DSP-TORCD
             MOVE   "C"      TO   EDIT-CURSOR  OF  DSP-TORCD
             MOVE   SPACE    TO   DSP-TORNM
         NOT INVALID KEY
*            MOVE  ZERO      TO   INVALID-FLG
             MOVE   ZERO     TO   ERR-MSG-CD
             MOVE   "D"      TO   EDIT-OPTION  OF  DSP-TORCD
             MOVE   " "      TO   EDIT-CURSOR  OF  DSP-TORCD
             MOVE   TOK-F02  TO   DSP-TORNM
     END-READ.
*
 TOKCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   出荷パターンマスタ存在チェック                *
*--------------------------------------------------------------*
 SPMCHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-SOKCD       TO   SPM-F01.
     MOVE    DSP-TORCD       TO   SPM-F02.
     MOVE    DSP-OTKBN       TO   SPM-F03.
     MOVE    DSP-CENKBN      TO   SPM-F04.
     READ    LNKSPML1
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TORCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-OTKBN
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CENKBN
         ELSE
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   " "     TO   EDIT-CURSOR  OF  DSP-SOKCD
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-TORCD
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-OTKBN
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CENKBN
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SOKCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-TORCD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-OTKBN
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CENKBN
         ELSE
              PERFORM        FILE-SUB
         END-IF
     END-IF.
*
 SPMCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイル→画面セット                         *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
     MOVE    SPM-F05          TO   DSP-SPTNCD.
     MOVE    SPM-F06          TO   DSP-KOTEI1.
     MOVE    SPM-F07          TO   DSP-KOTEI2.
     MOVE    SPM-F08          TO   DSP-KOTEI3.
     MOVE    SPM-F09          TO   DSP-KOTEI4.
     MOVE    SPM-F10          TO   DSP-KOTEI5.
*
     IF      WK-SYORI  =   2
             MOVE  "2"        TO   MAIN-FLG
     ELSE
             MOVE  "3"        TO   MAIN-FLG
     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力                               *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
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
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "F009"
                  MOVE   "2"          TO   MAIN-FLG
                  MOVE   ZERO         TO   ERR-MSG-CD
        WHEN  "E000"
                  PERFORM        BODYCHK-SUB
                  IF   ERR-MSG-CD      =   ZERO
                       MOVE   "4"     TO   MAIN-FLG
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-SPTNCD
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-SPTNCD
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-KOTEI1
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-KOTEI1
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-KOTEI2
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-KOTEI2
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-KOTEI3
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-KOTEI3
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-KOTEI4
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-KOTEI4
                       MOVE   "D"     TO
                              EDIT-OPTION  OF  DSP-KOTEI5
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-KOTEI5
*                      MOVE   "D"     TO
*                             EDIT-OPTION  OF  DSP-SOKNM1
*                      MOVE   SPACE   TO
*                             EDIT-CURSOR  OF  DSP-SOKNM1
*                      MOVE   "D"     TO
*                             EDIT-OPTION  OF  DSP-UNTEN
*                      MOVE   SPACE   TO
*                             EDIT-CURSOR  OF  DSP-UNTEN
*                      MOVE   "D"     TO
*                             EDIT-OPTION  OF  DSP-SYUSND
*                      MOVE   SPACE   TO
*                             EDIT-CURSOR  OF  DSP-SYUSND
*                      MOVE   "D"     TO
*                             EDIT-OPTION  OF  DSP-TEGSND
*                      MOVE   SPACE   TO
*                             EDIT-CURSOR  OF  DSP-TEGSND
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
     MOVE  SPACE        TO   DSP-MAS002.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ入力チェック                          *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
     IF  DSP-SPTNCD     =  SPACE
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SPTNCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SPTNCD
     ELSE
         MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SPTNCD
         MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SPTNCD
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
*
 FILPRT-01.
     IF      WK-SYORI   =    1
             MOVE   SPACE           TO   SPM-REC
             INITIALIZE                  SPM-REC
             MOVE   DSP-SOKCD       TO   SPM-F01
             MOVE   DSP-TORCD       TO   SPM-F02
             MOVE   DSP-OTKBN       TO   SPM-F03
             MOVE   DSP-CENKBN      TO   SPM-F04
             MOVE   DSP-SPTNCD      TO   SPM-F05
             MOVE   DSP-KOTEI1      TO   SPM-F06
             MOVE   DSP-KOTEI2      TO   SPM-F07
             MOVE   DSP-KOTEI3      TO   SPM-F08
             MOVE   DSP-KOTEI4      TO   SPM-F09
             MOVE   DSP-KOTEI5      TO   SPM-F10
             MOVE   PARA-BUMONCD    TO   SPM-F92
             MOVE   PARA-TANCD(1:2) TO   SPM-F93
             MOVE   SYS-DATE        TO   SPM-F94
             MOVE   WK-TIME(1:6)    TO   SPM-F95
             MOVE   SPACE           TO   SPM-F96
             MOVE   SPACE           TO   SPM-F97
             MOVE   ZERO            TO   SPM-F98
             MOVE   ZERO            TO   SPM-F99
     END-IF.
*
 FILPRT-02.
     IF      WK-SYORI   =    2
             MOVE   DSP-SPTNCD      TO   SPM-F05
             MOVE   DSP-KOTEI1      TO   SPM-F06
             MOVE   DSP-KOTEI2      TO   SPM-F07
             MOVE   DSP-KOTEI3      TO   SPM-F08
             MOVE   DSP-KOTEI4      TO   SPM-F09
             MOVE   DSP-KOTEI5      TO   SPM-F10
             MOVE   PARA-BUMONCD    TO   SPM-F96
             MOVE   PARA-TANCD(1:2) TO   SPM-F97
             MOVE   SYS-DATE        TO   SPM-F98
             MOVE   WK-TIME(1:6)    TO   SPM-F99
     END-IF.
*
 FILPRT-02.
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
        WHEN  1
*                  MOVE       SPM-REC  TO  DATA-TAIHI
                   WRITE      SPM-REC
                   END-WRITE
        WHEN  2
*                  MOVE       SPM-REC  TO  DATA-TAIHI
                   REWRITE    SPM-REC
                   END-REWRITE
        WHEN  3
*                  MOVE       SPM-REC  TO  DATA-TAIHI
                   DELETE     LNKSPML1
                   END-DELETE
     END-EVALUATE.
*
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE   "1"         TO   MAIN-FLG.
*
 FILPRT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF  LNKSPML1 ZSOKMS1 TOKMS2.
 END-END.
     EXIT.
