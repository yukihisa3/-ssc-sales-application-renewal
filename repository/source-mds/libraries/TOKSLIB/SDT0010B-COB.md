# SDT0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SDT0010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿             *
*    業務名　　　　　　　：　営業所データ連携                  *
*    モジュール名　　　　：　計上データ重複チェック            *
*    作成日／更新日　　　：　2000/06/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　営業所より受信したデータの重複    *
*                            チェックを行う。                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SDT0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/06/08.
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
*---<<  ダブリチェックＦ  >>---*
     SELECT  CHKF      ASSIGN     TO        DA-01-VI-CHKF
                       ORGANIZATION         IS   INDEXED
                       ACCESS     MODE      IS   RANDOM
                       RECORD     KEY       IS   CHK-F01
                                                 CHK-F02
                                                 CHK-F03
                                                 CHK-F04
                                                 CHK-F05
                                                 CHK-F06
                       FILE       STATUS    IS   CHK-ST.
*---<<  受信データ　>>---*
     SELECT  JYUF      ASSIGN               DA-01-S-JYUF
                       FILE      STATUS     IS   JYU-ST.
*---<<  エラーデータ　>>---*
     SELECT  ERRF      ASSIGN               DA-01-S-ERRF
                       FILE      STATUS     IS   ERR-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ダブリチェックＦ  >>---*
 FD  CHKF.
     COPY        DTCKUOF   OF        XFDLIB
                 JOINING   CHK       PREFIX.
*---<<  受信データ  >>---*
 FD  JYUF        BLOCK     CONTAINS  4    RECORDS
                 LABEL     RECORD    IS   STANDARD.
     COPY        DTCKUOF   OF        XFDLIB
                 JOINING   JYU       PREFIX.
*---<<  エラーデータ  >>---*
 FD  ERRF        BLOCK     CONTAINS  4    RECORDS
                 LABEL     RECORD    IS   STANDARD.
     COPY        DTCKUOF   OF        XFDLIB
                 JOINING   ERR       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  CHK-ST              PIC  X(02).
     03  JYU-ST              PIC  X(02).
     03  ERR-ST              PIC  X(02).
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  CHKF-INV-FLG        PIC  X(03)  VALUE SPACE.
     03  CHK-CNT             PIC  9(06)  VALUE ZERO.
     03  JYU-CNT             PIC  9(06)  VALUE ZERO.
     03  ERR-CNT             PIC  9(06)  VALUE ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  CHK-ERR           PIC N(15) VALUE
                        NC"ダブリチェックＦエラー".
     03  JYU-ERR           PIC N(15) VALUE
                        NC"受信ＤＴエラー".
     03  ERR-ERR           PIC N(15) VALUE
                        NC"エラーＤＴエラー".
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
****************************************************************
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 CHK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE CHKF.
     MOVE        CHK-ST      TO        E-ST.
     MOVE        "CHKF   "   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     CHK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYU-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JYUF.
     MOVE        JYU-ST      TO        E-ST.
     MOVE        "JYUF    "  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     JYU-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 ERR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ERRF.
     MOVE        ERR-ST      TO        E-ST.
     MOVE        "ERRF    "  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     ERR-ERR     UPON      CONS.
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
*ファイルＯＰＥＮ
     OPEN     I-O       CHKF.
     OPEN     INPUT     JYUF.
     OPEN     OUTPUT    ERRF.
*受信Ｆ初期読込み
     PERFORM  JYUF-READ-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"     TO     S-NAME.
*ダブリチェックファイル読込み
     MOVE      JYU-F01       TO     CHK-F01.
     MOVE      JYU-F02       TO     CHK-F02.
     MOVE      JYU-F03       TO     CHK-F03.
     MOVE      JYU-F04       TO     CHK-F04.
     MOVE      JYU-F05       TO     CHK-F05.
     MOVE      JYU-F06       TO     CHK-F06.
     PERFORM   CHKF-READ-SEC.
*存在チェック
     IF        CHKF-INV-FLG  =  "INV"
               MOVE JYU-REC  TO     CHK-REC
               WRITE                CHK-REC
               ADD  1        TO     CHK-CNT
     ELSE
               MOVE JYU-REC  TO     ERR-REC
               WRITE                ERR-REC
               ADD  1        TO     ERR-CNT
     END-IF.
*受信データ読込み
     PERFORM   JYUF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    受信データ読込み                                          *
****************************************************************
 JYUF-READ-SEC          SECTION.
*
     MOVE     "JYUF-READ-SEC" TO     S-NAME.
*ファイル読込み
     READ      JYUF   AT  END
               MOVE   "END"   TO     END-FLG
               NOT  AT  END
               ADD     1      TO     JYU-CNT
     END-READ.
*
 JYUF-READ-EXIT.
     EXIT.
****************************************************************
*    ダブリチェックＦ読込み                                    *
****************************************************************
 CHKF-READ-SEC          SECTION.
*
     MOVE     "CHKF-READ-SEC" TO     S-NAME.
*ファイル読込み
     READ      CHKF
               INVALID      MOVE   "INV"   TO   CHKF-INV-FLG
               NOT INVALID  MOVE   SPACE   TO   CHKF-INV-FLG
     END-READ.
*
 CHKF-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE    CHKF  JYUF  ERRF.
*
     DISPLAY "##ｼﾞｭｼﾝDT ｶｳﾝﾄ = " JYU-CNT " ##" UPON CONS.
     DISPLAY "##ｺｳｼﾝDT  ｶｳﾝﾄ = " CHK-CNT " ##" UPON CONS.
     DISPLAY "##ｴﾗｰDT   ｶｳﾝﾄ = " ERR-CNT " ##" UPON CONS.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
