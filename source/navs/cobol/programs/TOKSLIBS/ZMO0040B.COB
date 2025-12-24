****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　作業実績ファイル消込　            *
*    作成日／更新日　　　：　93/05/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　作業実績Ｆを読み，作業完成日と取消*
*                            フラグの条件を判定し，該当データを*
*                            削除する．　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZMO0040B.
*AUTHER.                NAV.
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
****<< 作業実績ファイル >>**********************************
     SELECT   ZSGYODT   ASSIGN  TO   DA-01-VI-ZSGYODT1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   SEQUENTIAL
                        RECORD  KEY          IS   SGYO-F01
                                                  SGYO-F02
                        FILE    STATUS       IS   SGYO-STATUS.
****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN  TO   DA-01-VI-JYOKEN1
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   JYO-F01
                                                  JYO-F02
                        FILE    STATUS       IS   JYO-STATUS.
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<< 作業実績ファイル >>**********************************
 FD    ZSGYODT
       LABEL  RECORD    IS      STANDARD.
       COPY   ZSGYODT   OF      XFDLIB
              JOINING   SGYO    PREFIX.
****<< 条件ファイル >>**************************************
 FD    HJYOKEN
       LABEL  RECORD    IS      STANDARD.
       COPY   HJYOKEN   OF      XFDLIB
              JOINING   JYO     PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  カウント  エリア        ****
 01  CNT-AREA                VALUE    ZERO.
     02 SGYO-CNT             PIC  9(07).
     02 SELECT-CNT           PIC  9(07).
     02 DELETE-CNT           PIC  9(07).
     02 SKIP-CNT             PIC  9(07).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SGYO-STATUS           PIC  X(2).
     02 JYO-STATUS            PIC  X(2).
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  ワーク                  ****
 01  WK-SHIMEBI              PIC  9(06)V9(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZMO0040B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  ZSGYODT.
     MOVE   "ZSGYODT "        TO    ERR-FL-ID.
     MOVE    SGYO-STATUS      TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
 ZMO0040B-START         SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG  =    "END".
     PERFORM       END-SEC.
     STOP     RUN.
 ZMO0040B-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       ZSGYODT.
     OPEN     INPUT     HJYOKEN.
     INITIALIZE         CNT-AREA    WK-SHIMEBI.
     MOVE     99              TO    JYO-F01.
     MOVE     "ZAI"           TO    JYO-F02.
     READ     HJYOKEN
         INVALID
              DISPLAY  "(ZMO0040B) ｴﾗｰﾒｯｾｰｼﾞ "     UPON CONS
              DISPLAY  " HJYOKEN READ INVALID-KEY" UPON CONS
              DISPLAY  "   JYO-F01 = " JYO-F01     UPON CONS
              DISPLAY  "   JYO-F02 = " JYO-F02     UPON CONS
              MOVE     "END"        TO     END-FLG
              GO                    TO     INIT-END
         NOT  INVALID
              MOVE      JYO-F05     TO     WK-SHIMEBI
              ADD       1           TO     WK-SHIMEBI
     END-READ.
     PERFORM  SGYO-READ-SEC.
 INIT-END.
     EXIT.
************************************************************
*      _１     作業実績ファイルＲＥＡＤ処理               *
************************************************************
 SGYO-READ-SEC          SECTION.
     READ     ZSGYODT
       AT  END
         MOVE     "END"      TO     END-FLG
         GO        TO        SGYO-READ-END
     END-READ.
     ADD      1              TO     SGYO-CNT.
     IF  SGYO-F97  =   "1"
         OR     SGYO-F05(1:6)   =   WK-SHIMEBI(1:6)
       THEN
         ADD       1         TO     SELECT-CNT
       ELSE
         ADD       1         TO     SKIP-CNT
         GO                  TO     SGYO-READ-SEC
     END-IF.
 SGYO-READ-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC               SECTION.
     DELETE   ZSGYODT.
     ADD      1              TO     DELETE-CNT.
     PERFORM  SGYO-READ-SEC.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    ZSGYODT   HJYOKEN.
     DISPLAY "(ZMO0040B)ﾆｭｳﾘｮｸ ｹﾝｽｳ = " SGYO-CNT   UPON  CONS.
     DISPLAY "(ZMO0040B)ﾖﾐﾄﾊﾞｼ ｹﾝｽｳ = " SKIP-CNT   UPON  CONS.
     DISPLAY "(ZMO0040B)ﾁｭｳｼｭﾂ ｹﾝｽｳ = " SELECT-CNT UPON  CONS.
     DISPLAY "(ZMO0040B)ｻｸｼﾞｮ  ｹﾝｽｳ = " DELETE-CNT UPON  CONS.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
