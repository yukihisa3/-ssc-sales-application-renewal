****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷業務　　　　　　　　　　　　　*
*    モジュール名　　　　：　出荷業務　サブ商品変換ＴＢＬ作成　*
*    作成日／更新日　　　：　18/08/30                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　基本ＣＳＶを読み、サブ商品変換ＴＢ*
*                            Ｌを作成する。                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSM9999B.
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
****<< 商品変換テーブル >>************************************
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
****<< サブ商品変換テーブル >>********************************
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F01
                                                 SUB-F02
                        FILE      STATUS    IS   SUB-STATUS.
****<< サブ用登録データ >>************************************
     SELECT   SUBTBLWK   ASSIGN    TO        DA-01-VS-SUBTBLWK
                        FILE      STATUS    IS   SWK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 商品変換テーブル >>*************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
****<< サブ商品変換テーブル >>*********************************
 FD  SUBTBLF.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
****<< 変換後商品変換TBL>>*********************************
 FD  SUBTBLWK.
     COPY     SUBTBLWK    OF        XFDLIB
              JOINING   SWK       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 TBL-STATUS           PIC  X(02).
     02 SUB-STATUS           PIC  X(02).
     02 SWK-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSM9999B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  SUBTBLF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HSHOTBL-END             PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*
 LINKAGE                SECTION.
 01  PARA-BUMON              PIC  X(02).
 01  PARA-TANCD              PIC  X(02).
************************************************************
 PROCEDURE              DIVISION USING PARA-BUMON
                                       PARA-TANCD.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HSHOTBL.
     MOVE   "HSHOTBL "        TO    ERR-FL-ID.
     MOVE    TBL-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBTBLF.
     MOVE   "SUBTBLL1"        TO    ERR-FL-ID.
     MOVE    SUB-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUBTBLWK.
     MOVE   "SUBTBLWK"        TO    ERR-FL-ID.
     MOVE    SWK-STATUS       TO    ERR-STCD.
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
*
     OPEN         INPUT     SUBTBLWK.
     OPEN         INPUT     HSHOTBL.
     OPEN         I-O       SUBTBLF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
     PERFORM  SUBTBLWK-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  SWK-F01        TO   TBL-F01.
     MOVE  SWK-F02        TO   TBL-F02.
     READ  HSHOTBL
           INVALID      MOVE  "INV"   TO  HSHOTBL-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  HSHOTBL-INV-FLG
     END-READ.
*
     MOVE  SWK-F01        TO   SUB-F01.
     MOVE  SWK-F02        TO   SUB-F02.
     READ  SUBTBLF
           INVALID      MOVE  "INV"   TO  SUBTBLF-INV-FLG
           NOT INVALID  MOVE  SPACE   TO  SUBTBLF-INV-FLG
     END-READ.
*
     IF    HSHOTBL-INV-FLG = SPACE
     AND   SUBTBLF-INV-FLG = "INV"
           MOVE   SPACE        TO   SUB-REC
           INITIALIZE               SUB-REC
           MOVE   TBL-F01      TO   SUB-F01
           MOVE   TBL-F02      TO   SUB-F02
           MOVE   TBL-F031     TO   SUB-F031
           MOVE   TBL-F0321    TO   SUB-F0321
           MOVE   TBL-F0322    TO   SUB-F0322
           MOVE   TBL-F0323    TO   SUB-F0323
           MOVE   TBL-F04      TO   SUB-F04
           MOVE   TBL-F05      TO   SUB-F05
           MOVE   TBL-F06      TO   SUB-F06
           MOVE   TBL-F07      TO   SUB-F07
           MOVE   TBL-F08      TO   SUB-F08
           MOVE   TBL-F09      TO   SUB-F09
           MOVE   TBL-F10      TO   SUB-F10
           MOVE   TBL-F11      TO   SUB-F11
           MOVE   TBL-F12      TO   SUB-F12
           MOVE   TBL-F02      TO   SUB-F13
           MOVE   SWK-F04      TO   SUB-F15
           MOVE   PARA-TANCD   TO   SUB-F93  SUB-F97
           MOVE   PARA-BUMON   TO   SUB-F92  SUB-F96
           MOVE   SYS-DATEW    TO   SUB-F94  SUB-F98
           MOVE   SYS-HHMMSS   TO   SUB-F95  SUB-F99
           WRITE  SUB-REC
           ADD    1            TO   OUT-CNT1
     ELSE
           ADD    1            TO   OUT-CNT2
     END-IF.
*
     PERFORM SUBTBLWK-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            商品変換テーブルの読込処理
************************************************************
 SUBTBLWK-READ-SEC                   SECTION.
*
     READ   SUBTBLWK
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SUBTBLWK-READ-EXIT
     END-READ.
     ADD     1         TO        IN-CNT.
*
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "## IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 SUBTBLWK-READ-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             HSHOTBL  SUBTBLWK.
*
     DISPLAY "* SUBTBLWK(INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* SUBTBLF(OUTPUT) = " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* NG-CNT          = " OUT-CNT2 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
