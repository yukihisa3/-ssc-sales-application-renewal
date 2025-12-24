# ZMO0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/ZMO0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫抽出上　　　　　              *
*    作成日／更新日　　　：　93/05/14                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　画面から条件を入力し、在庫マスタ　*
*                          から中間ファイルに抽出する          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0010B.
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
     SELECT   ZAIK      ASSIGN    TO        DA-01-VI-ZZAIMS1
                        ORGANIZATION        IS   INDEXED
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F031
                                                 ZAI-F032
                                                 ZAI-F033
                        FILE      STATUS    IS   ZAI-STATUS.
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
 COPY     ZMO0010           OF   XMDLIB.
****<< 在庫マスタ >>***************************************
 FD  ZAIK.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 条件ファイル >>*************************************
 FD  JYOK.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****<< 抽出済在庫マスタ >>*********************************
 FD  ZAIWK.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
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
 01  ZAIK-END                PIC  X(03).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
*
 01  WORK-AREA.
     03  H-SHORI             PIC  9(02).
     03  JYO-HENKAN          PIC  9(09)V99.
     03  JYO-HENKAN-R        REDEFINES     JYO-HENKAN.
         05  JYO-HEN1        PIC  9(07).
         05  JYO-HEN2        PIC  9(02).
         05  JYO-HEN3        PIC  9(02).
*
 01  KEY-AREA.
     03  H-SSOKCD            PIC  X(03).
     03  H-ESOKCD            PIC  X(03).
     03  H-SSHOCD            PIC  X(08).
     03  H-ESHOCD            PIC  X(08).
     03  H-STANA.
         05  H-STANA1        PIC  X(01).
         05  H-STANA2        PIC  X(03).
         05  H-STANA3        PIC  X(01).
         05  H-STANA4        PIC  X(01).
     03  H-ETANA.
         05  H-ETANA1        PIC  X(01).
         05  H-ETANA2        PIC  X(03).
         05  H-ETANA3        PIC  X(01).
         05  H-ETANA4        PIC  X(01).
*
 01  MSG-AREA.
     03  GID1                    PIC  N(30)     VALUE
         NC"：_再入力、_処理終了".
     03  GID2                    PIC  N(30)     VALUE
         NC"：_再入力、_処理終了　_前項戻".
     03  ERR1                    PIC  N(30)     VALUE
         NC"無効ＰＦキーです".
     03  ERR2                    PIC  N(30)     VALUE
         NC"入力項目にエラーがあります".
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0010B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAIK.
     MOVE   "ZZAIMS  "        TO    ERR-FL-ID.
     MOVE    ZAI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOK.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZAIWK.
     MOVE   "ZZAIWK  "        TO    ERR-FL-ID.
     MOVE    ZWK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
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
     OPEN         INPUT     ZAIK.
     OPEN         OUTPUT    ZAIWK.
*       システム日付の取得
     ACCEPT       SYSYMD    FROM     DATE.
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
     MOVE    "ZMO0010"           TO   DSP-FORMAT.
     MOVE    LOW-VALUE           TO   ZMO0010.
     PERFORM           DSP-C-INIT.
     PERFORM           9000-DSP-WRITE.
*       条件ファイルの読込処理
     MOVE    65        TO   JYO-F01.
     MOVE    DSP-WSNO  TO   JYO-F02.
     READ    JYOK
        INVALID   KEY
             INITIALIZE          JYO-REC
     END-READ.
     INITIALIZE                  KEY-AREA.
     IF      JYO-F04   =    01
             MOVE      02        TO   SHORI-F
             GO        TO        210-DSP-INIT-EXT
     END-IF.
*
     MOVE    JYO-F04        TO   JYO-HENKAN.
     MOVE    JYO-HEN2       TO   SSOKCD    H-SSOKCD
                                 ESOKCD    H-ESOKCD.
     MOVE    03             TO   SHORI-F.
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
                            EDIT-CURSOR    OF   STANA4
                            EDIT-CURSOR    OF   ETANA1
                            EDIT-CURSOR    OF   ETANA2
                            EDIT-CURSOR    OF   ETANA3
                            EDIT-CURSOR    OF   ETANA4.
     MOVE    "M"       TO   EDIT-OPTION    OF   SSOKCD
                            EDIT-OPTION    OF   ESOKCD
                            EDIT-OPTION    OF   SSHOCD
                            EDIT-OPTION    OF   ESHOCD
                            EDIT-OPTION    OF   STANA1
                            EDIT-OPTION    OF   STANA2
                            EDIT-OPTION    OF   STANA3
                            EDIT-OPTION    OF   STANA4
                            EDIT-OPTION    OF   ETANA1
                            EDIT-OPTION    OF   ETANA2
                            EDIT-OPTION    OF   ETANA3
                            EDIT-OPTION    OF   ETANA4.
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
*       倉庫コード
     MOVE    SSOKCD              TO   H-SSOKCD.
     IF      ESOKCD    =    LOW-VALUE
             MOVE      HIGH-VALUE     TO   H-ESOKCD
     ELSE
             MOVE      ESOKCD         TO   H-ESOKCD
     END-IF.
     IF      H-SSOKCD  >    H-ESOKCD
             MOVE      "ERR"     TO   ERR-FLG
             MOVE      ERR2      TO   ERRMSG
             MOVE      "R"       TO   EDIT-OPTION  OF  SSOKCD
                                      EDIT-OPTION  OF  ESOKCD
             MOVE      "C"       TO   EDIT-CURSOR  OF  SSOKCD
             GO        TO        DATA-CHK-EXT
     END-IF.
*
 SHOHIN-CHK.
*       商品コード
     MOVE    SSHOCD              TO   H-SSHOCD.
     IF      ESHOCD    =    LOW-VALUE
             MOVE      HIGH-VALUE     TO   H-ESHOCD
     ELSE
             MOVE      ESHOCD         TO   H-ESHOCD
     END-IF.
     IF      H-SSHOCD  >    H-ESHOCD
             MOVE      "ERR"     TO   ERR-FLG
             MOVE      ERR2      TO   ERRMSG
             MOVE      "R"       TO   EDIT-OPTION  OF  SSHOCD
                                      EDIT-OPTION  OF  ESHOCD
             MOVE      "C"       TO   EDIT-CURSOR  OF  SSHOCD
             GO        TO        DATA-CHK-EXT
     END-IF.
*       _番
     IF      STANA1    =    LOW-VALUE      AND
             STANA2    =    LOW-VALUE      AND
             STANA3    =    LOW-VALUE      AND
             STANA4    =    LOW-VALUE
             MOVE      LOW-VALUE      TO   H-STANA
     ELSE
        IF   STANA1    =    LOW-VALUE
             MOVE      SPACE          TO   H-STANA1
        ELSE
             MOVE      STANA1         TO   H-STANA1
        END-IF
        IF   STANA2    =    LOW-VALUE
             MOVE      SPACE          TO   H-STANA2
        ELSE
             MOVE      STANA2         TO   H-STANA2
        END-IF
        IF   STANA3    =    LOW-VALUE
             MOVE      SPACE          TO   H-STANA3
        ELSE
             MOVE      STANA3         TO   H-STANA3
        END-IF
        IF   STANA4    =    LOW-VALUE
             MOVE      SPACE          TO   H-STANA4
        ELSE
             MOVE      STANA4         TO   H-STANA4
        END-IF
     END-IF.
     IF      ETANA1    =    LOW-VALUE      AND
             ETANA2    =    LOW-VALUE      AND
             ETANA3    =    LOW-VALUE      AND
             ETANA4    =    LOW-VALUE
             MOVE      HIGH-VALUE     TO   H-ETANA
     ELSE
        IF   ETANA1    =    LOW-VALUE
             MOVE      SPACE          TO   H-ETANA1
        ELSE
             MOVE      ETANA1         TO   H-ETANA1
        END-IF
        IF   ETANA2    =    LOW-VALUE
             MOVE      SPACE          TO   H-ETANA2
        ELSE
             MOVE      ETANA2         TO   H-ETANA2
        END-IF
        IF   ETANA3    =    LOW-VALUE
             MOVE      SPACE          TO   H-ETANA3
        ELSE
             MOVE      ETANA3         TO   H-ETANA3
        END-IF
        IF   ETANA4    =    LOW-VALUE
             MOVE      SPACE          TO   H-ETANA4
        ELSE
             MOVE      ETANA4         TO   H-ETANA4
        END-IF
     END-IF.
     IF      H-STANA   >    H-ETANA
             MOVE      "ERR"     TO   ERR-FLG
             MOVE      ERR2      TO   ERRMSG
             MOVE      "R"       TO   EDIT-OPTION  OF   STANA1
                                      EDIT-OPTION  OF   STANA2
                                      EDIT-OPTION  OF   STANA3
                                      EDIT-OPTION  OF   STANA4
                                      EDIT-OPTION  OF   ETANA1
                                      EDIT-OPTION  OF   ETANA2
                                      EDIT-OPTION  OF   ETANA3
                                      EDIT-OPTION  OF   ETANA4
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
     MOVE    SPACE               TO   ZAIK-END.
     MOVE    H-SSOKCD            TO   ZAI-F01.
     MOVE    H-SSHOCD            TO   ZAI-F021.
     MOVE    LOW-VALUE           TO   ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033.
     START   ZAIK      KEY       >=   ZAI-F01
                                      ZAI-F021
                                      ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033
        INVALID   KEY
             MOVE      "END"     TO   ZAIK-END
        NOT  INVALID   KEY
             PERFORM        ZAIK-READ
     END-START.
*
     PERFORM
        UNTIL     ZAIK-END  =    "END"
             MOVE      ZAI-REC        TO   ZWK-REC
             WRITE     ZWK-REC
             PERFORM   ZAIK-READ
     END-PERFORM.
*
 SELECT-RTN-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 ZAIK-READ                       SECTION.
     READ    ZAIK
        AT   END
             MOVE      "END"     TO   ZAIK-END
             GO        TO        ZAIK-READ-EXT
     END-READ.
*
     IF      ZAI-F01   <    H-SSOKCD
             GO        TO        ZAIK-READ
     ELSE
        IF   ZAI-F01   >    H-ESOKCD
             MOVE     "END"      TO   ZAIK-END
             GO       TO         ZAIK-READ-EXT
        END-IF
     END-IF.
*
     IF      ZAI-F021  <    H-SSHOCD  OR
             ZAI-F021  >    H-ESHOCD
             GO        TO        ZAIK-READ
     END-IF.
*
     IF      ZAI-F03   <    H-STANA   OR
             ZAI-F03   >    H-ETANA
             GO        TO        ZAIK-READ
     END-IF.
 ZAIK-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             ZAIK      ZAIWK.
     CLOSE             JYOK      DSPF.
 300-END-SEC-EXT.
     EXIT.
*==============================================================*
*            画　面　表　示                                    *
*==============================================================*
 9000-DSP-WRITE                  SECTION.
     MOVE    SPACE               TO      DSP-PROC.
     MOVE    "SCREEN"            TO      DSP-GROUP.
     WRITE   ZMO0010.
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
