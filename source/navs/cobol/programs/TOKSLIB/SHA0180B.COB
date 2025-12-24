****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　発注管理（自動）                  *
*    業務名　　　　　　　：　自動発注数計算                    *
*    モジュール名　　　　：　自動発注区分クリア                *
*    作成日／更新日　　　：　2000/07/12                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件Ｆの自動発注区分をクリアする　*
*                                                              *
*                                                              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0180B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< 条件ファイル >>-*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
************************************************************
*     作業領域                                             *
************************************************************
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG            PIC  X(03)  VALUE SPACE.
*
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0180B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-JYO                 SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*      1.0     初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイル読込み
     OPEN    I-O        HJYOKEN.
     DISPLAY  "*** SHA0180B START ***"            UPON   CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*      2.0     メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*
*    条件ファイル検索(自動発注区分）
     MOVE    SPACE               TO   JYO-REC.
     INITIALIZE                       JYO-REC.
     MOVE    82                  TO   JYO-F01.
     MOVE    "JIDO"              TO   JYO-F02.
     READ    HJYOKEN  INVALID
             MOVE    "END"       TO   END-FLG
             DISPLAY "HJYOKEN INVALID KEY = " JYO-F01 ":" JYO-F02
                      UPON CONS
             GO                  TO   MAIN-EXIT
     END-READ.
     MOVE    ZERO                TO   JYO-F04.
     REWRITE JYO-REC.
*
     MOVE    "END"               TO   END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    HJYOKEN.
*
     DISPLAY  "*** SHA0180B END   ***"            UPON   CONS.
*
 END-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************

