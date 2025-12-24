****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ストックＮＯ外しリカバリ　　　　　*
*    モジュール名　　　　：　ストックＮＯ外しリカバリ　　　　　*
*    作成日／更新日　　　：　07/02/01                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　指定のストックＮＯ範囲の日付を指定*
*    の日付に変更する。また。計上区分も初期化する。　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0299B.
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
****<< 作業実績ファイル >>************************************
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SGY-F01
                                            SGY-F02
                        FILE      STATUS    IS   SGY-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 作業実績ファイル  >>************************************
 FD  SGYFILF            LABEL     RECORD     IS  STANDARD.
     COPY     SGYFILF   OF   XFDLIB    JOINING   SGY  AS   PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SGY-STATUS           PIC  X(02).
*システム日付の編集
 01  SYS-DATE          PIC 9(06).
 01  SYS-DATEW         PIC 9(08).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(8).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0299B".
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
 01  ZAMZAIF-END             PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
     03  GYO-CNT             PIC  9(02)   VALUE  1.
 01  WK-DENNO                PIC  9(07)   VALUE  0.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
************************************************************
 PROCEDURE             DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SGYFILF.
     MOVE   "SGYFILF "        TO    ERR-FL-ID.
     MOVE    SGY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP    RUN.
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
     OPEN         I-O       SGYFILF.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATEW.
*
     MOVE      SPACE              TO   SGY-REC.
     INITIALIZE                        SGY-REC.
     MOVE      191275             TO   SGY-F01.
*****MOVE      6311               TO   SGY-F01.
     START  SGYFILF  KEY  IS  >=  SGY-F01  SGY-F02
            INVALID
            DISPLAY NC"対象データ無し" UPON CONS
            MOVE    "END"         TO   END-FLG
            GO                    TO   100-INIT-END
     END-START.
*    作業実績ファイル読込み
     PERFORM  SGYFILF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*    作業完成日をセット
     MOVE      20070101   TO   SGY-F05.
*    計上区分解除
     MOVE      ZERO       TO   SGY-F13.
*    更新
     REWRITE  SGY-REC.
     ADD       1          TO   OUT-CNT1.
*
     PERFORM SGYFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 SGYFILF-READ-SEC                   SECTION.
*
     READ    SGYFILF
        NEXT   AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SGYFILF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
     IF   IN-CNT(5:3) = "500" OR "000"
          DISPLAY "READ-CNT = " IN-CNT  UPON CONS
     END-IF.
*
     IF      SGY-F01  >  194483
*****IF      SGY-F01  >  6326
             MOVE      "END"     TO   END-FLG
             GO        TO        SGYFILF-READ-EXT
     END-IF.
*
     IF      SGY-F05  >  20061231
             GO        TO        SGYFILF-READ-SEC
     END-IF.
*
 SGYFILF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             SGYFILF.
*
     DISPLAY "* SGYFILF (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* SGYFILF (OUTPUT)= " OUT-CNT1 " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
