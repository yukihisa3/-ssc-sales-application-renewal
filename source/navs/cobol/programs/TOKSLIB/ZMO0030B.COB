****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫マスタ月次繰越                *
*    作成日／更新日　　　：　93/05/14                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタの月次繰越処理を　　*
*                          行う                                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0030B.
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
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 在庫マスタ >>***************************************
 FD  ZAIK.
     COPY     ZZAIMS    OF        XFDLIB
              JOINING   ZAI       PREFIX.
****<< 条件ファイル >>*************************************
 FD  JYOK.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 ZAI-STATUS           PIC  X(02).
     02 JYO-STATUS           PIC  X(02).
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
     03  WK-SIMEMM           PIC  9(09)V99.
     03  WK-SIMEMM-R         REDEFINES      WK-SIMEMM.
         05  SIME-YY         PIC  9(07).
         05  SIME-MM         PIC  9(02).
         05  SIME-SS         PIC  9(02).
     03  IX                  PIC  9(02).
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
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0030B".
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
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           INIT-SEC.
     PERFORM           MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC                        SECTION.
     OPEN         I-O       ZAIK.
     OPEN         INPUT     JYOK.
*       システム日付の取得
     ACCEPT       SYSYMD    FROM     DATE.
*
     PERFORM      JYO-SYORI.
     IF  END-FLG  =   SPACE
         MOVE    SPACE           TO   ZAI-F01
                                      ZAI-F021
                                      ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033
         START   ZAIK      KEY   >=   ZAI-F01
                                      ZAI-F021
                                      ZAI-F022
                                      ZAI-F031
                                      ZAI-F032
                                      ZAI-F033
           INVALID   KEY
                MOVE      "END"     TO   END-FLG
           NOT  INVALID   KEY
                PERFORM        ZAI-READ
         END-START
     END-IF.
 INIT-SEC-EXIT.
     EXIT.
*==========================================================*
*      1.1      条件ファイル処理                           *
*==========================================================*
 JYO-SYORI                       SECTION.
     MOVE         99             TO   JYO-F01.
     MOVE        "ZAI"           TO   JYO-F02.
     READ    JYOK
        INVALID  KEY
             MOVE      "END"     TO   END-FLG
        NOT INVALID  KEY
             MOVE      JYO-F05   TO   WK-SIMEMM
     END-READ.
     DISPLAY   SIME-YY  UPON CONS.
     DISPLAY   SIME-MM  UPON CONS.
     IF  SIME-MM  <    1    OR
         SIME-MM  >   12
         MOVE     "END"          TO   END-FLG
         GO       TO             JYO-SYORI-EXIT
     END-IF.
     IF  SIME-MM  >    4    AND
         SIME-MM  <   12
         COMPUTE  IX   =    SIME-MM   -   5     +    1
     ELSE
         IF  SIME-MM    =   12
             MOVE       8         TO   IX
         ELSE
             COMPUTE   IX    =    SIME-MM   +    8
         END-IF
     END-IF.
 JYO-SYORI-EXIT.
     EXIT.
************************************************************
*      2.0      主処理　                                   *
************************************************************
 MAIN-SEC                        SECTION.

     MOVE    ZAI-F07             TO   ZAI-F203.
     MOVE    ZAI-F10             TO   ZAI-F201.
     MOVE    ZAI-F11             TO   ZAI-F202.
     MOVE    ZAI-F06             TO   ZAI-F07.
     COMPUTE ZAI-F07    =        ZAI-F06 - ZAI-F17 + ZAI-F18.
     COMPUTE ZAI-F08    =        ZAI-F17 - ZAI-F18.
*****MOVE    ZERO                TO   ZAI-F08.
*****MOVE    ZAI-F09             TO   ZAI-F161(IX).
     MOVE    ZAI-F204            TO   ZAI-F161(IX).
     MOVE    ZAI-F205            TO   ZAI-F204.
     MOVE    ZERO                TO   ZAI-F205.
*****
     MOVE    ZAI-F10             TO   ZAI-F162(IX).
     MOVE    ZAI-F11             TO   ZAI-F163(IX).
     MOVE    ZAI-F17             TO   ZAI-F10.
     MOVE    ZAI-F18             TO   ZAI-F11.
     MOVE    ZERO                TO   ZAI-F09.
     MOVE    ZERO                TO   ZAI-F17.
     MOVE    ZERO                TO   ZAI-F18.
*****MOVE    ZAI-F19             TO   ZAI-F08.
     MOVE    ZERO                TO   ZAI-F19.
     REWRITE                     ZAI-REC.
     PERFORM                     ZAI-READ.
 MAIN-SEC-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                         SECTION.
     CLOSE             JYOK      ZAIK.
 END-SEC-EXT.
     EXIT.
*==========================================================*
*            在庫マスタの読込処理
*==========================================================*
 ZAI-READ                        SECTION.
     READ    ZAIK
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        ZAI-READ-EXT
     END-READ.
 ZAI-READ-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
