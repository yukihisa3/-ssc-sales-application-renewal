****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷（ＬＩＮＫＳ連携）　　　　　　*
*    モジュール名　　　　：　カインズカテゴリーマスタ保守　　　*
*    作成日／更新日　　　：　2014/07/18                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　マスタ登録・修正・削除            *
*    更新日　　　　　　　：　    /  /                          *
*    　　　　　　　　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0520I.
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
*---<<  カインズカテゴリーマスタ  >>---*
     SELECT   CNZCTEL1  ASSIGN    TO        DA-01-VI-CNZCTEL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   CTE-F01
                        FILE      STATUS    IS   CTE-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FBT05201  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  カインズカテゴリーマスタ  >>---*
 FD  CNZCTEL1.
     COPY     CNZCTEL1  OF        XFDLIB
              JOINING   CTE       PREFIX.
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
     02  CTE-STATUS          PIC  X(02).
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBT0520I".
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
            NC"　　　　　　　　　　　　　　　　".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"　　　　　　　　　".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"　　　　　　　　　　　　　　　　　".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"　　　　　　　　　　　".
     02  MSG-ERR10           PIC  N(28) VALUE
            NC"　　　　　　　　　　　　　".
     02  MSG-ERR11           PIC  N(28) VALUE
            NC"　　　　　　　　　".
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
     DISPLAY  MSG-ABEND1     UPON CONS.
     DISPLAY  MSG-ABEND2     UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    CNZCTEL1.
     MOVE     "CNZCTEL1"     TO   ERR-FL-ID.
     MOVE     CTE-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON CONS.
     DISPLAY  MSG-ABEND2     UPON CONS.
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
*
     OPEN    I-O             DSPF     CNZCTEL1.
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
     MOVE    "FBT05201"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FBT05201.
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
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CTECD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CTECD.
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
     WRITE    DSP-FBT05201.
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
*    MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CTECD.
*    MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CTECD.
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
           MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CTECD
           MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CTECD
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
                                EDIT-OPTION  OF  DSP-CTECD
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-CTECD
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
*    MOVE  SPACE        TO   DSP-MAS001.
     MOVE  SPACE        TO   DSP-CTECD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*
*   カインズカテゴリーマスタ存在チェック
     PERFORM   CTECHK-SUB.
     IF        ERR-MSG-CD    =   ZERO
               CONTINUE
     ELSE
               GO  TO        HEADCHK-END
     END-IF.
*
 HEADCHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.1   カインズカテゴリーマスタ存在チェック          *
*--------------------------------------------------------------*
 CTECHK-SUB             SECTION.
     MOVE    ZERO            TO   ERR-MSG-CD.
     MOVE    DSP-CTECD       TO   CTE-F01.
     READ    CNZCTEL1
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CTECD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-CTECD
         ELSE
              MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CTECD
              MOVE   " "     TO   EDIT-CURSOR  OF  DSP-CTECD
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CTECD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-CTECD
         ELSE
              PERFORM        FILE-SUB
         END-IF
     END-IF.
*
 CTECHK-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2.2    ファイル→画面セット                         *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
*
*   カテゴリ名
     MOVE    CTE-F02          TO   DSP-CTENM.
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
                              EDIT-OPTION  OF  DSP-CTENM
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-CTENM
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
*    MOVE  SPACE        TO   DSP-MAS002.
     MOVE  SPACE        TO   DSP-CTENM.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.3.2     ＢＯＤＹ入力チェック                          *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     MOVE    ZERO            TO   ERR-MSG-CD.
*    IF  DSP-CTENM      =  SPACE
*        MOVE   04      TO   ERR-MSG-CD
*        MOVE   "R"     TO   EDIT-OPTION  OF  DSP-CTENM
*        MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-CTENM
*    ELSE
*        MOVE   "D"     TO   EDIT-OPTION  OF  DSP-CTENM
*        MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-CTENM
*    END-IF.
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
*   登録モード時
     IF      WK-SYORI   =    1
             MOVE   SPACE           TO   CTE-REC
             INITIALIZE                  CTE-REC
*           カテゴリコード
             MOVE   DSP-CTECD       TO   CTE-F01
*           カテゴリ名
             MOVE   DSP-CTENM       TO   CTE-F02
*           登録担当者部門
             MOVE   PARA-BUMONCD    TO   CTE-F92
*           登録担当者
             MOVE   PARA-TANCD(1:2) TO   CTE-F93
*           登録日
             MOVE   SYS-DATE        TO   CTE-F94
*           登録時刻
             MOVE   WK-TIME(1:6)    TO   CTE-F95
*           更新担当者部門
             MOVE   SPACE           TO   CTE-F96
*           更新担当者
             MOVE   SPACE           TO   CTE-F97
*           更新日
             MOVE   ZERO            TO   CTE-F98
*           更新時刻
             MOVE   ZERO            TO   CTE-F99
     END-IF.
*
 FILPRT-02.
*   修正モード時
     IF      WK-SYORI   =    2
*           カテゴリコード
             MOVE   DSP-CTENM       TO   CTE-F02
*           更新担当者部門
             MOVE   PARA-BUMONCD    TO   CTE-F96
*           更新担当者
             MOVE   PARA-TANCD(1:2) TO   CTE-F97
*           更新日
             MOVE   SYS-DATE        TO   CTE-F98
*           更新時刻
             MOVE   WK-TIME(1:6)    TO   CTE-F99
     END-IF.
*
 FILPRT-02.
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
*      登録モード時
        WHEN  1
*                  MOVE       CTE-REC  TO  DATA-TAIHI
                   WRITE      CTE-REC
                   END-WRITE
*      修正モード時
        WHEN  2
*                  MOVE       CTE-REC  TO  DATA-TAIHI
                   REWRITE    CTE-REC
                   END-REWRITE
*      削除モード時
        WHEN  3
*                  MOVE       CTE-REC  TO  DATA-TAIHI
                   DELETE     CNZCTEL1
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
     CLOSE    DSPF  CNZCTEL1.
 END-END.
     EXIT.
