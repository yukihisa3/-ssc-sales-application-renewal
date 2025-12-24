# SFU3898B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3898B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替情報　消し込み　　　      *
*    作成日／作成者　　　：　2016/12/20  INOUE                 *
*    処理概要　　　　　　：　規定保存期間を経過した社内振替　　*
*                            データの削除　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SFU3898B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  振替情報ファイル  >>---*
     SELECT   SFRHEDL3  ASSIGN    TO             SFRHEDL3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HED-F24
                                                 HED-F14
                        FILE      STATUS    IS   HED-STATUS.
*
*---<<  振替明細ファイル  >>---*
     SELECT   SFRMEIL1  ASSIGN    TO             SFRMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   MEI-F01
                                                 MEI-F02
                                                 MEI-F03
                                                 MEI-F04
                                                 MEI-F05
                                                 MEI-F06
                                                 MEI-F07
                                                 MEI-F08
                                                 MEI-F11
                                                 MEI-F12
                        FILE      STATUS    IS   MEI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  振替情報ファイル  >>---*
 FD  SFRHEDL3
                        LABEL     RECORD    IS   STANDARD.
                        COPY      SFRHEDL3  OF   XFDLIB
                        JOINING   HED       AS   PREFIX.
*---<<  振替明細ファイル  >>---*
 FD  SFRMEIL1
                        LABEL     RECORD    IS   STANDARD.
                        COPY      SFRMEIL1  OF   XFDLIB
                        JOINING   MEI       AS   PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  HED-STATUS          PIC  X(02).
     03  MEI-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  HED-END-FLG         PIC  X(03)  VALUE SPACE.
     03  MEI-END-FLG         PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  HED-READ-CNT        PIC  9(07)  VALUE ZERO.
     03  HED-DEL-CNT         PIC  9(07)  VALUE ZERO.
     03  MEI-READ-CNT        PIC  9(07)  VALUE ZERO.
     03  MEI-DEL-CNT         PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     03  WRK-DATE1           PIC  9(06).
     03  WRK-DATE1R          REDEFINES   WRK-DATE1.
         05  WRK-DATE1R1     PIC  9(04).
         05  WRK-DATE1R2     PIC  9(02).
     03  WRK-DATE2           PIC  9(06).
 01  WK-SIME                 PIC  9(06)  VALUE ZERO.
 01  WK-KIKAN                PIC  9(02)  VALUE ZERO.
 01  WK-TUKI                 PIC S9(02)  VALUE ZERO.
 01  WK-DEL-DATE.
     03  DEL-DATE1.
         05  DEL-DATE1-1     PIC  9(04)  VALUE ZERO.
         05  DEL-DATE1-2     PIC  9(02)  VALUE ZERO.
     03  DEL-DATE2           PIC  9(02)  VALUE ZERO.
 01  WK-CHK01                PIC  9(02)  VALUE ZERO.
 01  WK-CHK02                PIC  9(02)  VALUE ZERO.
 01  WK-HED-F14              PIC  9(08)  VALUE ZERO.
*
 01  WK-F01                  PIC  X(02)  VALUE SPACE.
 01  WK-F02                  PIC  9(04)  VALUE ZERO.
 01  WK-F03                  PIC  X(02)  VALUE SPACE.
 01  WK-F04                  PIC  X(02)  VALUE SPACE.
 01  WK-F05                  PIC  X(08)  VALUE SPACE.
 01  WK-F06                  PIC  X(05)  VALUE SPACE.
 01  WK-F07                  PIC  X(02)  VALUE SPACE.
 01  WK-F08                  PIC  X(01)  VALUE SPACE.
*
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SFU3898B".
       05  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE  " ###".
*
 LINKAGE                SECTION.
 01  LINK-IN-DEL-DATE      PIC   9(08).
*
*-------------------------------------------------------------*
*       0.0   エラー処理                                      *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION
                             USING LINK-IN-DEL-DATE.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SFRHEDL3.
     MOVE     "SFRHEDL3"     TO   ERR-FL-ID.
     MOVE     HED-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SFRMEIL1.
     MOVE     "SFRMEIL1"     TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*-------------------------------------------------------------*
*       1.0   コントロール                                    *
*-------------------------------------------------------------*
 CONTROL-SEC                 SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC   UNTIL  HED-END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
*-------------------------------------------------------------*
*       2.0   初期処理                                        *
*-------------------------------------------------------------*
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             SFRHEDL3
                             SFRMEIL1.
*
     DISPLAY LINK-IN-DEL-DATE NC"以前を削除します" UPON CONS
*
*振替情報ファイルＳＴＡＲＴ
     PERFORM HED-START-SEC.
     IF      "END"     =  HED-END-FLG
             DISPLAY
             NC"＃＃　振替情報ファイル削除対象無　＃＃"
                                          UPON CONS
             GO     TO    INIT-END
     END-IF.
*
*振替情報ファイル読込み
     PERFORM HED-READ-SEC.
     IF      "END"     =  HED-END-FLG
             DISPLAY
             NC"＃＃　振替情報ファイル削除対象無　＃＃"
                                          UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
*-------------------------------------------------------------*
*      振替情報ファイルＳＴＡＲＴ                             *
*-------------------------------------------------------------*
 HED-START-SEC                SECTION.
*
     MOVE    "1"               TO   HED-F24.
     MOVE    ZERO              TO   HED-F14.
     START   SFRHEDL3   KEY IS >=   HED-F24  HED-F14
             INVALID
             MOVE   "END"      TO   HED-END-FLG
             GO                TO   HED-START-EXIT
     END-START.
*
 HED-START-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      振替情報ファイル読込　　　                             *
*-------------------------------------------------------------*
 HED-READ-SEC                SECTION.
*
     READ    SFRHEDL3
             AT  END
             MOVE   "END"      TO   HED-END-FLG
             GO                TO   HED-READ-EXIT
     END-READ.
*振替情報ファイルの入荷予定日が削除基準日を超えたら終了
     MOVE    HED-F14           TO   WK-HED-F14.
     IF      WK-HED-F14    >   LINK-IN-DEL-DATE
             MOVE   "END"      TO   HED-END-FLG
             GO                TO   HED-READ-EXIT
     END-IF.
*経過件数表示
     ADD     1                 TO     HED-READ-CNT.
     IF      HED-READ-CNT(5:3) =    "000"  OR  "500"
             DISPLAY "READ-CNT =    " HED-READ-CNT UPON CONS
     END-IF.
*
 HED-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      3.0　　メイン処理                                      *
*-------------------------------------------------------------*
 MAIN-SEC                    SECTION.
*
 MAIN-00.
*ＫＥＹ保管
     MOVE    HED-F01     TO      WK-F01.
     MOVE    HED-F02     TO      WK-F02.
     MOVE    HED-F03     TO      WK-F03.
     MOVE    HED-F04     TO      WK-F04.
     MOVE    HED-F05     TO      WK-F05.
     MOVE    HED-F06     TO      WK-F06.
     MOVE    HED-F07     TO      WK-F07.
     MOVE    HED-F08     TO      WK-F08.
*
 MAIN-01.
*振替情報ファイル削除
     DELETE   SFRHEDL3.
     ADD      1            TO     HED-DEL-CNT.
*
 MAIN-02.
*振替明細ファイルＳＴＡＲＴ
     MOVE    SPACE         TO     MEI-END-FLG.
*
     PERFORM MEI-START-SEC.
     IF      "END"     =  MEI-END-FLG
*T↓
*            DISPLAY
*            NC"＃＃　振替明細ファイル削除対象無　＃＃"
*                                         UPON CONS
*T↑
             GO     TO    MAIN-05
     END-IF.
*
 MAIN-03.
*振替明細ファイル読込み
     PERFORM MEI-READ-SEC.
     IF      "END"     =  MEI-END-FLG
*T↓
*            DISPLAY
*            NC"＃＃　振替明細ファイル削除対象無　＃＃"
*                                         UPON CONS
*T↑
             GO     TO    MAIN-05
     END-IF.
*
 MAIN-04.
*振替明細ファイル削除
     DELETE   SFRMEIL1.
     ADD      1            TO     MEI-DEL-CNT.
     GO                    TO     MAIN-03.
*
 MAIN-05.
*振替情報ファイル読込み
     PERFORM HED-READ-SEC.
*
 MAIN-END.
     EXIT.
*-------------------------------------------------------------*
*      振替明細ファイルＳＴＡＲＴ                             *
*-------------------------------------------------------------*
 MEI-START-SEC                SECTION.
*
     MOVE    WK-F01            TO   MEI-F01.
     MOVE    WK-F02            TO   MEI-F02.
     MOVE    WK-F03            TO   MEI-F03.
     MOVE    WK-F04            TO   MEI-F04.
     MOVE    WK-F05            TO   MEI-F05.
     MOVE    WK-F06            TO   MEI-F06.
     MOVE    WK-F07            TO   MEI-F07.
     MOVE    WK-F08            TO   MEI-F08.
     MOVE    " "               TO   MEI-F11.
     MOVE    ZERO              TO   MEI-F12.
*
     START   SFRMEIL1  KEY IS  >=   MEI-F01 MEI-F02 MEI-F03
                                    MEI-F04 MEI-F05 MEI-F06
                                    MEI-F07 MEI-F08 MEI-F11
                                    MEI-F12
             INVALID
             MOVE   "END"      TO   MEI-END-FLG
             GO                TO   MEI-START-EXIT
     END-START.
*
 MEI-START-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      振替明細ファイル読込　　　                             *
*-------------------------------------------------------------*
 MEI-READ-SEC                SECTION.
*
     READ    SFRMEIL1
             AT  END
             MOVE   "END"      TO   MEI-END-FLG
             GO                TO   MEI-READ-EXIT
     END-READ.
*キーブレイクしたら終了
     IF  (   MEI-F01 NOT = WK-F01  )  OR
         (   MEI-F02 NOT = WK-F02  )  OR
         (   MEI-F03 NOT = WK-F03  )  OR
         (   MEI-F04 NOT = WK-F04  )  OR
         (   MEI-F05 NOT = WK-F05  )  OR
         (   MEI-F06 NOT = WK-F06  )  OR
         (   MEI-F07 NOT = WK-F07  )  OR
         (   MEI-F08 NOT = WK-F08  )
             MOVE   "END"             TO   MEI-END-FLG
             GO                       TO   MEI-READ-EXIT
     END-IF.
*経過件数表示
     ADD     1                     TO     MEI-READ-CNT.
     IF      MEI-READ-CNT(5:3)     =    "000"  OR  "500"
             DISPLAY "MEI-READ-CNT =    " MEI-READ-CNT UPON CONS
     END-IF.
*
 MEI-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      4.0        終了処理                                    *
*-------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE              SFRHEDL3  SFRMEIL1.
     DISPLAY  NC"削除基準日　　　＝" LINK-IN-DEL-DATE UPON CONS.
     DISPLAY  NC"振替情報Ｆ　ＩＮ＝" HED-READ-CNT     UPON CONS.
     DISPLAY  NC"　　　　　　削除＝" HED-DEL-CNT      UPON CONS.
     DISPLAY  NC"振替明細Ｆ　ＩＮ＝" MEI-READ-CNT     UPON CONS.
     DISPLAY  NC"　　　　　　削除＝" MEI-DEL-CNT      UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
