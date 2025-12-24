# SIT0050I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0050I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理システム                  *
*    業務名　　　　　　　：　仕入先マスタ　　　　　　　        *
*    モジュール名　　　　：　仕入先マスタ保守　                *
*    作成日／作成者　　　：　93/04/14  /NAV.                   *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*            新郵便番号追加                                    *
*    更新日／更新者　　　：　09/03/23  /IMAI                   *
*            ＩＴ統制対応                                      *
*    処理概要　　　　　　：ZMT0030O→SIT0050Iへ　　　　　　
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SIT0050I.
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
*---<<  仕入先マスタ  >>---*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STATUS.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        DA-01-VI-MSTLOGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F04
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FIT00501   OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  仕入先マスタ  >>---*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF        XFDLIB
              JOINING   SHI       PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
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
     02  SHI-STATUS          PIC  X(02).
     02  MSL-STATUS          PIC X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SIT0050I".
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
            NC"仕入先コードが未登録です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"仕入先コードが未入力です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"仕入先名称が未入力です".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  7   TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(250).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING PARA-BUMONCD
                                             PARA-TANCD
                                             PARA-UPDTDATE
                                             PARA-UPDTIME.
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
                     PROCEDURE    ZSHIMS.
     MOVE     "ZSHIMS"       TO   ERR-FL-ID.
     MOVE     SHI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    MSTLOGF.
     MOVE     "MSTLOGF"      TO   ERR-FL-ID.
     MOVE     MSL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP        RUN.
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
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
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
     OPEN    I-O             DSPF.
     OPEN    I-O             ZSHIMS  MSTLOGF.
     MOVE    "FIT00501"      TO   DSP-FORMAT.
     MOVE    SPACE           TO   DSP-FIT00501.
     MOVE    SPACE           TO   END-FLG.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    2               TO   WK-SYORI.
     MOVE    SPACE           TO   DSP-PROC.
*受渡しパラメタセット
     MOVE     DATE-AREA      TO   PARA-UPDTDATE.
     MOVE     WK-TIME(1:6)   TO   PARA-UPDTIME.
*
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
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SIRCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SIRCD.
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
     WRITE    DSP-FIT00501.
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
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-SIRNM1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-SIRNM1.
     MOVE   "D"     TO   EDIT-OPTION  OF  DSP-KAKNIN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-KAKNIN.
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
                         MOVE   "D"     TO
                                EDIT-OPTION  OF  DSP-SIRCD
                         MOVE   SPACE   TO
                                EDIT-CURSOR  OF  DSP-SIRCD
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
     MOVE  SPACE        TO   DSP-SIRCD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*      2.2.2     ＨＥＡＤ部の入力チェック                      *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*
     IF  DSP-SIRCD      =    SPACE
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SIRCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SIRCD
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
     MOVE    DSP-SIRCD       TO   SHI-F01.
     READ    ZSHIMS
         INVALID   KEY
             MOVE  1         TO   INVALID-FLG
         NOT INVALID KEY
             MOVE  ZERO      TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG    =    1
         IF   WK-SYORI   NOT =   1
              MOVE   03      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SIRCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SIRCD
         END-IF
     ELSE
* ＮＯＴ ＩＮＶＡＬＩＤ処理
         IF   WK-SYORI       =    1
              MOVE   02      TO   ERR-MSG-CD
              MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SIRCD
              MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SIRCD
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
     MOVE    SHI-F02          TO   DSP-SIRNM1.
     MOVE    SHI-F03          TO   DSP-SIRNM2.
     MOVE    SHI-F04          TO   DSP-TANBK1.
     MOVE    SHI-F05          TO   DSP-TANBK2.
*--------------- 99/09/28追加 -----------------------*
     MOVE    SHI-F121         TO   DSP-NYBN1.
     MOVE    SHI-F122         TO   DSP-NYBN2.
*----------------------------------------------------*
     MOVE    SHI-F06          TO   DSP-YUBIN.
     MOVE    SHI-F07          TO   DSP-JUSYO1.
     MOVE    SHI-F08          TO   DSP-JUSYO2.
     MOVE    SHI-F09          TO   DSP-TELNO.
     MOVE    SHI-F10          TO   DSP-FAXNO.
     MOVE    SHI-F11          TO   DSP-KENCD.
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
                              EDIT-OPTION  OF  DSP-SIRNM1
                       MOVE   SPACE   TO
                              EDIT-CURSOR  OF  DSP-SIRNM1
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
     IF  DSP-SIRNM1     =  SPACE
         MOVE   07      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  DSP-SIRNM1
         MOVE   "C"     TO   EDIT-CURSOR  OF  DSP-SIRNM1
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
              MOVE   SPACE   TO   DSP-KAKNIN
        WHEN  "F009"
              IF   WK-SYORI  =    3
                   MOVE   01      TO   ERR-MSG-CD
              ELSE
                   MOVE   "3"     TO   MAIN-FLG
              END-IF
              MOVE   SPACE   TO   DSP-KAKNIN
        WHEN  "E000"
              IF  DSP-KAKNIN  NOT  =  "Y"
                  MOVE   06      TO   ERR-MSG-CD
              ELSE
                  MOVE   SPACE   TO   DSP-KAKNIN
                  MOVE   "5"     TO   MAIN-FLG
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
     IF      WK-SYORI   =    1
             MOVE   SPACE         TO   SHI-REC
             INITIALIZE      SHI-REC
     END-IF.
     MOVE    DSP-SIRCD            TO   SHI-F01.
     MOVE    DSP-SIRNM1           TO   SHI-F02.
     MOVE    DSP-SIRNM2           TO   SHI-F03.
     MOVE    DSP-TANBK1           TO   SHI-F04.
     MOVE    DSP-TANBK2           TO   SHI-F05.
*--------------- 99/09/28追加 -----------------------*
     MOVE    DSP-NYBN1            TO   SHI-F121.
     MOVE    DSP-NYBN2            TO   SHI-F122.
*----------------------------------------------------*
     MOVE    DSP-YUBIN            TO   SHI-F06.
     MOVE    DSP-JUSYO1           TO   SHI-F07.
     MOVE    DSP-JUSYO2           TO   SHI-F08.
     MOVE    DSP-TELNO            TO   SHI-F09.
     MOVE    DSP-FAXNO            TO   SHI-F10.
     MOVE    DSP-KENCD            TO   SHI-F11.
*  処理モードにより追加・更新・削除
     EVALUATE    WK-SYORI
        WHEN  1
                   MOVE     SHI-REC    TO  DATA-TAIHI
                   WRITE    SHI-REC
                   END-WRITE
        WHEN  2
                   MOVE     SHI-REC    TO  DATA-TAIHI
                   REWRITE  SHI-REC
                   END-REWRITE
        WHEN  3
                   MOVE     SHI-REC    TO  DATA-TAIHI
                   DELETE   ZSHIMS
                   END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE   "1"         TO   MAIN-FLG.
 FILPRT-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      ZSHIMS  MSTLOGF.
 END-END.
     EXIT.
****************************************************************
*            マスタ更新履歴ファイル出力            *
****************************************************************
 MSTLOGF-WRITE-SEC           SECTION.
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
     ACCEPT    WK-TIME          FROM   TIME.
*
     MOVE     "02"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02.
     MOVE     PARA-TANCD          TO   MSL-F03.
     MOVE     WK-SYORI            TO   MSL-F04.
     MOVE     DATE-AREA           TO   MSL-F05.
     MOVE     WK-TIME(1:6)        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F04
                                           MSL-F05
                                           MSL-F06
              INVALID
              MOVE     "02"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     WK-SYORI      TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO    MSL-F07
              MOVE     DATA-TAIHI    TO    MSL-F08
              WRITE    MSL-REC
              GO TO    MSTLOGF-WRITE-EXIT
     END-START.
     MOVE     0                   TO   SEQ.
*
 MSTLOG-WRITE-010.
     READ   MSTLOGF  NEXT  AT        END
            GO             TO        MSTLOGF-WRITE-EXIT
     END-READ.
*
     IF  (MSL-F01 > "02")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > WK-SYORI)   OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "02"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     WK-SYORI         TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
         WRITE    MSL-REC
         GO       TO               MSTLOGF-WRITE-EXIT
     ELSE
         COMPUTE  SEQ =  MSL-F07  +  1
         GO       TO               MSTLOG-WRITE-010
     END-IF.
 MSTLOGF-WRITE-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
