# SBT0036B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0036B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ジュンテンドーＴＣ発注システム　　*
*    モジュール名　　　　：　修正伝票の連携ＦＬＧを解除する　　*
*    作成日／更新日　　　：　2012/10/11                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　バッチ_にてＴＣ伝票ファイルを読み*
*                            修正キーＦを索引して、ＦＬＧを削除*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0036B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/11.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT
     YA          IS     PITCH-2
     YB          IS     PITCH-15
     YB-21       IS     BAIKAKU.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＴＣ発注変換データ  >>---*
     SELECT   JTCDENL1  ASSIGN    TO        DA-01-VI-JTCDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HEN-F46  HEN-F47
                                                 HEN-F01  HEN-F48
                                                 HEN-F02  HEN-F04
                                                 HEN-F051 HEN-F07
                                                 HEN-F112 HEN-F03
                        FILE      STATUS    IS   HEN-ST.
*---<<  売上伝票修正キーＦ  >>---*
     SELECT   JTCHKYL1  ASSIGN    TO        DA-01-VI-JTCHKYL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HKY-F01  HKY-F02
                                                 HKY-F03  HKY-F04
                                                 HKY-F05  HKY-F06
                                                 HKY-F07  HKY-F08
                                                 HKY-F09  HKY-F10
                        FILE      STATUS    IS   HKY-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＣ発注変換ファイル  >>---*
 FD  JTCDENL1.
     COPY     JTCDENF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*---<<  売上伝票修正キーＦ  >>---* *
 FD  JTCHKYL1.
     COPY     JTCHKYF   OF        XFDLIB
              JOINING   HKY       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  HEN-ST              PIC  X(02).
     03  HKY-ST              PIC  X(02).
*カウント
 01  RD-CNT                  PIC  9(07)  VALUE ZERO.
 01  REW-CNT                 PIC  9(07)  VALUE ZERO.
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  JTCHKYF-INV-FLG     PIC  X(03)  VALUE SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HEN-ERR           PIC N(15) VALUE
                        NC"ＴＣ伝票Ｆ変換エラー".
     03  HKY-ERR           PIC N(15) VALUE
                        NC"ＴＣ伝票修正キーＦエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE               SECTION.
 01  LINK-HIDUKE           PIC 9(08).
 01  LINK-JIKAN            PIC 9(04).
 01  LINK-TORICD           PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  LINK-HIDUKE
                                         LINK-JIKAN
                                         LINK-TORICD.
**
 DECLARATIVES.
 HEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCDENL1.
     MOVE        HEN-ST      TO        E-ST.
     MOVE        "JTCDENL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 HKY-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JTCHKYL1.
     MOVE        HKY-ST      TO        E-ST.
     MOVE        "JTCHKYL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HKY-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC  UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       JTCDENL1.
     OPEN     INPUT     JTCHKYL1.
*END-FLG CLEAR
     MOVE      SPACE         TO     END-FLG.
*ＴＣ伝票Ｆスタート
     MOVE      SPACE         TO     HEN-REC.
     INITIALIZE                     HEN-REC.
     MOVE      LINK-HIDUKE   TO     HEN-F46.
     MOVE      LINK-JIKAN    TO     HEN-F47.
     MOVE      LINK-TORICD   TO     HEN-F01.
     START     JTCDENL1 KEY  IS  >=   HEN-F46  HEN-F47  HEN-F01
                                      HEN-F48  HEN-F02  HEN-F04
                                      HEN-F051 HEN-F07  HEN-F112
                                      HEN-F03
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃対象データ無１＃" UPON CONS
         GO                  TO     INIT-END
      NOT  INVALID
         PERFORM   JTCDENL1-READ-SEC
         IF   END-FLG = "END"
              DISPLAY NC"＃対象データ無２＃" UPON CONS
              GO                  TO     INIT-END
         END-IF
     END-START.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     PERFORM  JTCHKYL1-READ-SEC.
*ＩＮＶの場合、売上伝票修正キーファイルを作成する。
     DISPLAY "JTCHKYF-INV-FLG = " JTCHKYF-INV-FLG UPON CONS.
     IF JTCHKYF-INV-FLG  =  SPACE
        MOVE     SPACE            TO    HEN-F68
        REWRITE  HEN-REC
        ADD      1                TO    REW-CNT
     END-IF.
*
 MAIN-010.
     PERFORM  JTCDENL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE JTCDENL1 JTCHKYL1.
*データ作成件数出力
     DISPLAY "DATA READ CNT = " RD-CNT   UPON CONS.
     DISPLAY "DATA REWT CNT = " REW-CNT  UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*    ＴＣ伝票Ｆ読込　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 JTCDENL1-READ-SEC         SECTION.
*
     MOVE     "JTCDEDL1-READ-SEC" TO    S-NAME.
*
     READ      JTCDENL1  NEXT  AT  END
               MOVE      "END"    TO    END-FLG
               GO                 TO    JTCDENL1-READ-EXIT
     END-READ.
*
     ADD       1                  TO    RD-CNT.
*
     IF        LINK-HIDUKE  NOT =  HEN-F46
     OR        LINK-JIKAN   NOT =  HEN-F47
     OR        LINK-TORICD  NOT =  HEN-F01
               MOVE      "END"    TO    END-FLG
     END-IF.
*
 JTCDENL1-READ-EXIT.
     EXIT.
****************************************************************
*    売上伝票修正キーファイル更新　　　　　　　　　　　　　　　*
****************************************************************
 JTCHKYL1-READ-SEC         SECTION.
*
     MOVE     "JTCHKYL1-READ-SEC" TO    S-NAME.
*
     MOVE     HEN-F46             TO    HKY-F01.
     MOVE     HEN-F47             TO    HKY-F02.
     MOVE     HEN-F01             TO    HKY-F03.
     MOVE     HEN-F48             TO    HKY-F04.
     MOVE     HEN-F02             TO    HKY-F05.
     MOVE     HEN-F04             TO    HKY-F06.
     MOVE     HEN-F051            TO    HKY-F07.
     MOVE     HEN-F07             TO    HKY-F08.
     MOVE     HEN-F112            TO    HKY-F09.
     MOVE     ZERO                TO    HKY-F10.
*
     READ      JTCHKYL1  INVALID
               MOVE      "INV"    TO    JTCHKYF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    JTCHKYF-INV-FLG
     END-READ.
*
 JTCHKYL1-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
