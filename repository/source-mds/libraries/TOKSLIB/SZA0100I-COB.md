# SZA0100I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZA0100I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫抽出上　　　　　              *
*    作成日／更新日　　　：　00/05/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面から条件を入力し、在庫マスタ　*
*                          から中間ファイルに抽出する          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0100I.
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
****<<  画面ファイル        >>******************************
     SELECT   DSPF      ASSIGN  TO   GS-DSPF
                        ORGANIZATION         IS  SEQUENTIAL
                        ACCESS  MODE         IS  SEQUENTIAL
                        SYMBOLIC DESTINATION IS  "DSP"
                        PROCESSING MODE      IS   DSP-PROC
                        GROUP                IS   DSP-GROUP
                        FORMAT               IS   DSP-FORMAT
                        SELECTED FUNCTION    IS   DSP-FUNC
                        FILE STATUS          IS   DSP-STATUS
                        DESTINATION-1        IS   DSP-WSNO.
****<< 在庫マスタ >>************************************
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE      STATUS    IS   ZAI-STATUS.
****<< 倉庫マスタ　 >>**********************************
     SELECT   SOKM      ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
****<< 条件ファイル >>**********************************
     SELECT   JYOK      ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<< 抽出済在庫マスタ >>**********************************
     SELECT   ZAIWK     ASSIGN    TO        DA-01-S-ZZAIWK
                        FILE      STATUS    IS   ZWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<<  画面ファイル        >>******************************
 FD    DSPF.
     COPY     FZA01001  OF       XMDLIB.
****<< 在庫マスタ >>***************************************
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 倉庫マスタ >>***************************************
 FD  SOKM.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****<< 条件ファイル >>*************************************
 FD  JYOK.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 抽出済在庫マスタ >>*********************************
 FD  ZAIWK
              BLOCK CONTAINS  4   RECORDS.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
     02 SOK-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
     02 ZWK-STATUS           PIC  X(02).
****  画面制御項目            ****
 01  DSP-CONTROL.
     02 DSP-PROC             PIC  X(02).
     02 DSP-GROUP            PIC  X(08).
     02 DSP-FORMAT           PIC  X(08).
     02 DSP-STATUS           PIC  X(02).
     02 DSP-FUNC             PIC  X(04).
     02 DSP-WSNO             PIC  X(08).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  SHORI-F                 PIC  9(02).
 01  ERR-FLG                 PIC  X(03).
 01  INV-FLG                 PIC  9(01).
 01  ZAMZAIF-END             PIC  X(03).
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT             PIC  9(07)   VALUE  0.
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  WORK-AREA.
     03  H-SHORI             PIC  9(02).
*
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)  OCCURS  8.
 01  IX                      PIC  9(02).
*
 01  KEY-AREA.
     03  H-SSOKCD            PIC  X(02).
     03  H-ESOKCD            PIC  X(02).
     03  H-SSHOCD            PIC  X(08).
     03  H-ESHOCD            PIC  X(08).
     03  H-STANA.
         05  H-STANA1        PIC  X(01).
         05  H-STANA2        PIC  X(03).
         05  H-STANA3        PIC  X(02).
     03  H-ETANA.
         05  H-ETANA1        PIC  X(01).
         05  H-ETANA2        PIC  X(03).
         05  H-ETANA3        PIC  X(02).
*
 01  MSG-AREA.
     03  GID1                    PIC  N(30)     VALUE
         NC"：_取消　_終了".
     03  GID2                    PIC  N(30)     VALUE
         NC"：_取消　_終了　_戻り".
     03  ERR1                    PIC  N(30)     VALUE
         NC"無効ＰＦキーです".
     03  ERR2                    PIC  N(30)     VALUE
         NC"倉庫コードに誤りがあります".
     03  ERR3                    PIC  N(30)     VALUE
         NC"開始が終了を越えています".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0100I".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
************************************************************
 LINKAGE            SECTION.
 01  LINK-SOKCD         PIC  X(02).
 01  LINK-DSOKCD        PIC  X(02).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-SOKCD
                                              LINK-DSOKCD.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAMZAIF.
     MOVE   "ZAMZAIF "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SOKM.
     MOVE   "ZSOKMS  "        TO    ERR-FL-ID.
     MOVE    SOK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOK.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAIWK.
     MOVE   "ZZAIWK  "        TO    ERR-FL-ID.
     MOVE    ZWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
     OPEN         I-O       DSPF.
     OPEN         INPUT     JYOK.
     OPEN         INPUT     ZAMZAIF.
     OPEN         INPUT     SOKM.
     OPEN         OUTPUT    ZAIWK.
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
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     JYOK
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"    TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03             TO   HEN-TOKHAN
     END-READ.
*
     MOVE    01        TO   SHORI-F.
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
     EVALUATE     SHORI-F
         WHEN     01
             PERFORM             210-DSP-INIT
         WHEN     02
             PERFORM             220-DSP-DATA1
         WHEN     03
             PERFORM             230-DSP-DATA2
         WHEN     04
             PERFORM             240-DSP-ANS
     END-EVALUATE.
 200-MAIN-SEC-EXT.
     EXIT.
*----------------------------------------------------------*
*      210       画面の初期処理　　　                      *
*----------------------------------------------------------*
 210-DSP-INIT                    SECTION.
     MOVE    "FZA01001"          TO   DSP-FORMAT.
     MOVE    SPACE               TO   FZA01001.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    HEN-TIME            TO   STIME.
     MOVE    HEN-TOKHAN-AREA     TO   TOKHAN.
     PERFORM           DSP-C-INIT.
     PERFORM           9000-DSP-WRITE.
* 倉庫
     IF      LINK-DSOKCD  =  "01"  OR  "88"
             MOVE      02             TO   SHORI-F
     ELSE
             MOVE      03             TO   SHORI-F
             MOVE      LINK-SOKCD     TO   SSOKCD   ESOKCD
                                           H-SSOKCD H-ESOKCD
                                           SOK-F01
             PERFORM   SOK-READ
             IF        INV-FLG   =   ZERO
                       MOVE   SOK-F02      TO  SSOKNM  ESOKNM
             END-IF
     END-IF.
*
 210-DSP-INIT-EXT.
     EXIT.
*----------------------------------------------------------*
*                画面制御の初期化
*----------------------------------------------------------*
 DSP-C-INIT                      SECTION.
     MOVE    SPACE     TO   EDIT-CURSOR    OF   SSOKCD
                            EDIT-CURSOR    OF   ESOKCD
                            EDIT-CURSOR    OF   SSHOCD
                            EDIT-CURSOR    OF   ESHOCD
                            EDIT-CURSOR    OF   STANA1
                            EDIT-CURSOR    OF   STANA2
                            EDIT-CURSOR    OF   STANA3
                            EDIT-CURSOR    OF   ETANA1
                            EDIT-CURSOR    OF   ETANA2
                            EDIT-CURSOR    OF   ETANA3.
     MOVE    "M"       TO   EDIT-OPTION    OF   SSOKCD
                            EDIT-OPTION    OF   ESOKCD
                            EDIT-OPTION    OF   SSHOCD
                            EDIT-OPTION    OF   ESHOCD
                            EDIT-OPTION    OF   STANA1
                            EDIT-OPTION    OF   STANA2
                            EDIT-OPTION    OF   STANA3
                            EDIT-OPTION    OF   ETANA1
                            EDIT-OPTION    OF   ETANA2
                            EDIT-OPTION    OF   ETANA3.
 DSP-C-INIT-EXT.
     EXIT.
*----------------------------------------------------------*
*      220       画面の入力１の処理　　                    *
*----------------------------------------------------------*
 220-DSP-DATA1                   SECTION.
     MOVE    GID1                TO   PFMSG.
     PERFORM           9000-DSP-WRITE.
     PERFORM           9100-DSP-READ.
*
     EVALUATE     DSP-FUNC
        WHEN      "F004"    MOVE      01        TO   SHORI-F
        WHEN      "F005"    MOVE      99        TO   SHORI-F
                            MOVE      "END"     TO   END-FLG
        WHEN      "E000"    PERFORM        DATA-CHK
                            IF   ERR-FLG   =    SPACE
                                 MOVE      02        TO   H-SHORI
                                 MOVE      04        TO   SHORI-F
                            END-IF
        WHEN      OTHER     MOVE      ERR1      TO   ERRMSG
     END-EVALUATE.
 220-DSP-DATA1-EXT.
     EXIT.
*----------------------------------------------------------*
*      230       画面の入力２の処理　　                    *
*----------------------------------------------------------*
 230-DSP-DATA2                   SECTION.
     MOVE    GID1                TO   PFMSG.
     PERFORM           9000-DSP-WRITE.
     PERFORM           9100-DSP-READ.
*
     EVALUATE     DSP-FUNC
        WHEN      "F004"    MOVE      01        TO   SHORI-F
        WHEN      "F005"    MOVE      99        TO   SHORI-F
                            MOVE      "END"     TO   END-FLG
        WHEN      "E000"    PERFORM        DATA-CHK
                            IF   ERR-FLG   =    SPACE
                                 MOVE      03        TO   H-SHORI
                                 MOVE      04        TO   SHORI-F
                            END-IF
        WHEN      OTHER     MOVE      ERR1      TO   ERRMSG
     END-EVALUATE.
 230-DSP-DATA2-EXT.
     EXIT.
*----------------------------------------------------------*
*                入力条件のチェック
*----------------------------------------------------------*
 DATA-CHK                        SECTION.
     PERFORM           DSP-C-INIT.
     MOVE    SPACE               TO   ERR-FLG.
     IF      SHORI-F   =    03
             GO        TO        SHOHIN-CHK
     END-IF.
*
*倉庫コード
     MOVE    SPACE               TO   SSOKNM.
     MOVE    SSOKCD              TO   H-SSOKCD.
     IF      SSOKCD    NOT =   SPACE
             MOVE      SSOKCD         TO  SOK-F01
             PERFORM   SOK-READ
             IF        INV-FLG   =    ZERO
                       MOVE    SOK-F02    TO   SSOKNM
             ELSE
                       MOVE    "ERR"      TO   ERR-FLG
                       MOVE    ERR2       TO   ERRMSG
                       MOVE    "R"   TO   EDIT-OPTION  OF  SSOKCD
                       MOVE    "C"   TO   EDIT-CURSOR  OF  SSOKCD
             END-IF
     END-IF.
     MOVE    SPACE               TO   ESOKNM.
     MOVE    ESOKCD              TO   H-ESOKCD.
     IF      ESOKCD    NOT =   SPACE  AND       "99"
             MOVE      ESOKCD         TO  SOK-F01
             PERFORM   SOK-READ
             IF        INV-FLG   =    ZERO
                       MOVE    SOK-F02    TO   ESOKNM
             ELSE
                       MOVE    "ERR"      TO   ERR-FLG
                       MOVE    ERR2       TO   ERRMSG
                       MOVE    "R"   TO   EDIT-OPTION  OF  ESOKCD
                       MOVE    "C"   TO   EDIT-CURSOR  OF  ESOKCD
             END-IF
     ELSE
             MOVE     "99"            TO   ESOKCD
             MOVE      HIGH-VALUE     TO   H-ESOKCD
     END-IF.
     IF      ERR-FLG   =    SPACE     AND
             H-SSOKCD  >    H-ESOKCD
             MOVE      "ERR"     TO   ERR-FLG
             MOVE      ERR3      TO   ERRMSG
             MOVE      "R"       TO   EDIT-OPTION  OF  SSOKCD
                                      EDIT-OPTION  OF  ESOKCD
             MOVE      "C"       TO   EDIT-CURSOR  OF  SSOKCD
     END-IF.
*
 SHOHIN-CHK.
*商品コード
     IF       SSHOCD    NOT =  SPACE
              MOVE   SSHOCD       TO  WK-SHOCD
              PERFORM          UNTIL  WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IX FROM 7 BY -1 UNTIL IX = 0
                    MOVE   WK-SHO(IX)       TO  WK-SHO(IX + 1)
                 END-PERFORM
                 MOVE      ZERO             TO  WK-SHO(1)
              END-PERFORM
              MOVE  WK-SHOCD      TO  SSHOCD
     END-IF.
     IF       ESHOCD    NOT =  SPACE
              MOVE   ESHOCD       TO  WK-SHOCD
              PERFORM          UNTIL  WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IX FROM 7 BY -1 UNTIL IX = 0
                    MOVE   WK-SHO(IX)       TO  WK-SHO(IX + 1)
                 END-PERFORM
                 MOVE      ZERO             TO  WK-SHO(1)
              END-PERFORM
              MOVE  WK-SHOCD      TO  ESHOCD
     END-IF.
     MOVE    SSHOCD              TO   H-SSHOCD.
     IF      ESHOCD    =    SPACE
             MOVE     "99999999"      TO   ESHOCD
             MOVE      HIGH-VALUE     TO   H-ESHOCD
     ELSE
             MOVE      ESHOCD         TO   H-ESHOCD
     END-IF.
     IF      H-SSHOCD  >    H-ESHOCD
             IF        ERR-FLG   =   SPACE
                       MOVE      "ERR"     TO   ERR-FLG
                       MOVE      ERR3      TO   ERRMSG
             END-IF
             MOVE      "R"       TO   EDIT-OPTION  OF  SSHOCD
                                      EDIT-OPTION  OF  ESHOCD
             MOVE      "C"       TO   EDIT-CURSOR  OF  SSHOCD
     END-IF.
* 棚番
     MOVE    STANA1              TO   H-STANA1.
     MOVE    STANA2              TO   H-STANA2.
     MOVE    STANA3              TO   H-STANA3.
     IF      ETANA1    =    SPACE     AND
             ETANA2    =    SPACE     AND
             ETANA3    =    SPACE
             MOVE     "9"             TO   ETANA1
             MOVE     "999"           TO   ETANA2
             MOVE     "99"            TO   ETANA3
             MOVE      HIGH-VALUE     TO   H-ETANA
     ELSE
             MOVE      ETANA1         TO   H-ETANA1
             MOVE      ETANA2         TO   H-ETANA2
             MOVE      ETANA3         TO   H-ETANA3
     END-IF.
     IF      H-STANA   >    H-ETANA
             IF        ERR-FLG   =   SPACE
                       MOVE      "ERR"     TO   ERR-FLG
                       MOVE      ERR3      TO   ERRMSG
             END-IF
             MOVE      "R"       TO   EDIT-OPTION  OF   STANA1
                                      EDIT-OPTION  OF   STANA2
                                      EDIT-OPTION  OF   STANA3
                                      EDIT-OPTION  OF   ETANA1
                                      EDIT-OPTION  OF   ETANA2
                                      EDIT-OPTION  OF   ETANA3
             MOVE      "C"       TO   EDIT-CURSOR  OF   STANA1
     END-IF.
 DATA-CHK-EXT.
     EXIT.
*----------------------------------------------------------*
*      240       画面の確認処理　　
*----------------------------------------------------------*
 240-DSP-ANS                     SECTION.
     MOVE    GID2                TO   PFMSG.
     MOVE    "Y"                 TO   ANS.
     PERFORM           9000-DSP-WRITE.
     PERFORM           9100-DSP-READ.
*
     EVALUATE     DSP-FUNC
        WHEN      "F004"    MOVE      01        TO   SHORI-F
        WHEN      "F006"    MOVE      H-SHORI   TO   SHORI-F
        WHEN      "F005"    MOVE      99        TO   SHORI-F
                            MOVE      "END"     TO   END-FLG
        WHEN      "E000"    PERFORM        SELECT-RTN
                            MOVE      99        TO   SHORI-F
                            MOVE      "END"     TO   END-FLG
        WHEN      OTHER     MOVE      ERR1      TO   ERRMSG
     END-EVALUATE.
 240-DSP-ANS-EXT.
     EXIT.
************************************************************
*                抽出処理
************************************************************
 SELECT-RTN             SECTION.
*
     MOVE    SPACE               TO   ZAMZAIF-END.
     MOVE    H-SSOKCD            TO   ZAI-F01.
     MOVE    H-SSHOCD            TO   ZAI-F021.
     MOVE    LOW-VALUE           TO   ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033.
     START   ZAMZAIF      KEY    >=   ZAI-F01
                                      ZAI-F021
                                      ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033
        INVALID   KEY
             MOVE      "END"     TO   ZAMZAIF-END
        NOT  INVALID   KEY
             PERFORM        ZAMZAIF-READ
     END-START.
*
     PERFORM
        UNTIL     ZAMZAIF-END  =    "END"
             MOVE      ZAI-REC        TO   ZWK-REC
             WRITE     ZWK-REC
             ADD       1              TO   OUT-CNT
             PERFORM   ZAMZAIF-READ
     END-PERFORM.
*
 SELECT-RTN-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 ZAMZAIF-READ                       SECTION.
     READ    ZAMZAIF
        AT   END
             MOVE      "END"     TO   ZAMZAIF-END
             GO        TO        ZAMZAIF-READ-EXT
     END-READ.
     ADD     1         TO        IN-CNT.
*
     IF      ZAI-F01   >    H-ESOKCD
             MOVE     "END"      TO   ZAMZAIF-END
             GO       TO         ZAMZAIF-READ-EXT
     END-IF.
*
     IF      ZAI-F01   =    H-ESOKCD  AND
             ZAI-F021  >    H-ESHOCD
             MOVE     "END"      TO   ZAMZAIF-END
             GO       TO         ZAMZAIF-READ-EXT
     END-IF.
     IF      ZAI-F021  <    H-SSHOCD  OR
             ZAI-F021  >    H-ESHOCD
             GO        TO        ZAMZAIF-READ
     END-IF.
*
     IF      ZAI-F03   <    H-STANA   OR
             ZAI-F03   >    H-ETANA
             GO        TO        ZAMZAIF-READ
     END-IF.
 ZAMZAIF-READ-EXT.
     EXIT.
************************************************************
*            倉庫マスタの読込処理
************************************************************
 SOK-READ                           SECTION.
     READ    SOKM
        INVALID
             MOVE      1         TO   INV-FLG
        NOT INVALID
             MOVE      0         TO   INV-FLG
     END-READ.
 SOK-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             ZAMZAIF   ZAIWK    SOKM
                       JYOK      DSPF.
*
     DISPLAY "* ZAMZAIF (INPUT) = " IN-CNT  " *"  UPON CONS.
     DISPLAY "* ZZAIWK  (OUTPUT)= " OUT-CNT " *"  UPON CONS.
*
     IF      OUT-CNT  =  0
             MOVE    4050     TO    PROGRAM-STATUS
     END-IF.
 300-END-SEC-EXT.
     EXIT.
*==============================================================*
*            画　面　表　示                                    *
*==============================================================*
 9000-DSP-WRITE                  SECTION.
     MOVE    SPACE               TO      DSP-PROC.
     MOVE    "SCREEN"            TO      DSP-GROUP.
     WRITE   FZA01001.
     MOVE    SPACE               TO      ERRMSG
                                         ANS.
 9000-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*            画　面　入　力                                    *
*--------------------------------------------------------------*
 9100-DSP-READ                   SECTION.
     EVALUATE     SHORI-F
         WHEN     02        MOVE      "DATA1"   TO   DSP-GROUP
         WHEN     03        MOVE      "DATA2"   TO   DSP-GROUP
         WHEN     04        MOVE      "KAKU "   TO   DSP-GROUP
     END-EVALUATE.
     MOVE         "NE"           TO      DSP-PROC.
     READ    DSPF                AT      END
             GO                  TO      9100-DSP-EXIT
     END-READ.
     IF      DSP-STATUS      NOT =       ZERO
             GO                  TO      9100-DSP-READ
     END-IF.
 9100-DSP-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
