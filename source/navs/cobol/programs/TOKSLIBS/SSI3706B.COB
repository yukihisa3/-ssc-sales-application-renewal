****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成                    *
*    作成日／更新日　　　：　12/06/09                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＡＣＯＳディスカバラーデータを読み*
*                            請求合計ファイルを作成する。　　　*
*    （共通）　　　　                                          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI3706B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/06/09.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＩＮ１ファイル  >>---*
     SELECT   NAFUKOSE  ASSIGN    TO        DA-01-VI-NAFUKOS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   IN-F02
                                                 IN-F07
                                                 IN-F06
                        FILE      STATUS    IS   IN-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   SETGK90   ASSIGN    TO        DA-01-VI-SETGK903
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   OUT-F01
                                                 OUT-F03
                                                 OUT-F14
                                                 OUT-F04
                                                 OUT-F05
                        FILE      STATUS    IS   OUT-ST.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＩＮ１ファイル  >>---*
 FD  NAFUKOSE.
     COPY        NAFUKOSE  OF        XFDLIB
                 JOINING   IN        PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  SETGK90.
     COPY        SETGK90   OF        XFDLIB
                 JOINING   OUT       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  IN-ST               PIC  X(02).
     03  OUT-ST              PIC  X(02).
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  SETGK90-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT             PIC  9(07)  VALUE ZERO.
***  日付変換
 01  WK-HIDUKE               PIC  9(08).
 01  WK-HIDUKE-R             REDEFINES   WK-HIDUKE.
     03  WK-HIDUKE-YY        PIC  X(04).
     03  WK-HIDUKE-MM        PIC  X(02).
     03  WK-HIDUKE-DD        PIC  X(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME             PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE             PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  IN-ERR             PIC N(15) VALUE
         NC"ＡＣＯＳ側データエラー".
     03  OUT-ERR            PIC N(15) VALUE
         NC"ナフコ請求合計Ｆエラー".
*
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      NAFUKOSE.
     MOVE        IN-ST     TO        E-ST.
     MOVE       "NAFUKOSE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     IN-ERR    UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
**
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SETGK90.
     MOVE        OUT-ST    TO        E-ST.
     MOVE       "SETGK903" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     OUT-ERR   UPON      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END     DECLARATIVES.
****************************************************************
 GENERAL-PROCESS             SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 GENERAL-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     OPEN     INPUT     NAFUKOSE.
     OPEN     I-O       SETGK90.
     MOVE     SPACE          TO   END-FLG.
*
     PERFORM  NAFUKOSE-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     137607        TO   OUT-F01.
     MOVE     IN-F02        TO   OUT-F03.
     MOVE     WK-HIDUKE     TO   OUT-F14.
     MOVE     WK-HIDUKE     TO   OUT-F04.
     MOVE     IN-F07        TO   OUT-F05.
*請求合計ファイル索引
     PERFORM  SETGK90-READ-SEC.
*
     IF  SETGK90-INV-FLG  =  "INV"
         MOVE  SPACE             TO  OUT-REC
         INITIALIZE                  OUT-REC
         MOVE  137607            TO  OUT-F01
         MOVE  WK-HIDUKE         TO  OUT-F02
         MOVE  IN-F02            TO  OUT-F03
         MOVE  WK-HIDUKE         TO  OUT-F04
         MOVE  IN-F07            TO  OUT-F05
         MOVE  IN-F13            TO  OUT-F06
         MOVE  IN-F04            TO  OUT-F07
         MOVE  IN-F11            TO  OUT-F09
         MOVE  WK-HIDUKE         TO  OUT-F10
         MOVE  137607            TO  OUT-F11
         MOVE  WK-HIDUKE         TO  OUT-F13
         MOVE  WK-HIDUKE         TO  OUT-F14
         WRITE OUT-REC
         ADD   1                 TO  OUT-CNT
     ELSE
         IF  IN-F06  NOT =  1
             ADD   IN-F13        TO  OUT-F06
             ADD   IN-F11        TO  OUT-F09
             REWRITE  OUT-REC
             ADD   1             TO  OUT-CNT
         ELSE
             ADD   1                 TO  SKIP-CNT
             DISPLAY "#DUP-CNT  = " IN-CNT  " #"  UPON CONS
             DISPLAY "#OUT-F01  = " OUT-F01 " #"  UPON CONS
             DISPLAY "#OUT-F03  = " OUT-F03 " #"  UPON CONS
             DISPLAY "#OUT-F14  = " OUT-F14 " #"  UPON CONS
             DISPLAY "#OUT-F04  = " OUT-F04 " #"  UPON CONS
             DISPLAY "#OUT-F05  = " OUT-F05 " #"  UPON CONS
             DISPLAY "#DENKU    = " IN-F04  " #"  UPON CONS
         END-IF
     END-IF.
*
     PERFORM NAFUKOSE-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*    ＡＣＯＳディスカバラーファイル読込
****************************************************************
 NAFUKOSE-READ-SEC      SECTION.
*
     READ     NAFUKOSE
          AT  END
              MOVE     "END" TO   END-FLG
              GO             TO   NAFUKOSE-READ-EXIT
     END-READ.
*
     ADD      1              TO   IN-CNT.
*
     IF  IN-CNT(4:4)  =  "0000" OR "5000"
         DISPLAY "#IN-CNT   = " IN-CNT  " #"  UPON CONS
     END-IF.
*日付編集
     MOVE  IN-F17(1:4)       TO   WK-HIDUKE-YY.
     IF  IN-F17(7:1) = "/"
         MOVE  "0"           TO   WK-HIDUKE-MM(1:1)
         MOVE  IN-F17(6:1)   TO   WK-HIDUKE-MM(2:1)
     ELSE
         MOVE  IN-F17(6:2)   TO   WK-HIDUKE-MM
     END-IF.
     IF  IN-F17(9:1) = SPACE
         MOVE  "0"           TO   WK-HIDUKE-DD(1:1)
         MOVE  IN-F17(8:1)   TO   WK-HIDUKE-DD(2:1)
         GO                  TO   NAFUKOSE-READ-EXIT
     ELSE
         IF  IN-F17(10:1) = SPACE
             MOVE  "0"           TO   WK-HIDUKE-DD(1:1)
             MOVE  IN-F17(9:1)   TO   WK-HIDUKE-DD(2:1)
         ELSE
             MOVE  IN-F17(9:2)   TO   WK-HIDUKE-DD
         END-IF
     END-IF.
*
 NAFUKOSE-READ-EXIT.
     EXIT.
****************************************************************
*　　ナフコ請求合計ファイル読込
****************************************************************
 SETGK90-READ-SEC       SECTION.
*
     READ     SETGK90
          INVALID KEY
              MOVE  "INV"        TO   SETGK90-INV-FLG
          NOT INVALID KEY
              MOVE  SPACE        TO   SETGK90-INV-FLG
     END-READ.
*
 SETGK90-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE    NAFUKOSE  SETGK90.
*
     DISPLAY "#####################"  UPON CONS.
     DISPLAY "#IN-CNT   = " IN-CNT    " #"  UPON CONS.
     DISPLAY "#SKIP-CNT = " SKIP-CNT  " #"  UPON CONS.
     DISPLAY "#OUT-CNT  = " OUT-CNT   " #"  UPON CONS.
     DISPLAY "#####################"  UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
