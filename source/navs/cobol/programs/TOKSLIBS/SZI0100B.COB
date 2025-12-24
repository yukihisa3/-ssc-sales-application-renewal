****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　　　　　　　*
*    モジュール名　　　　：　在庫ＥＸＣＥＬ月次消し込み        *
*    作成日／作成者　　　：　2016/06/06  INOUE                 *
*    処理概要　　　　　　：　月次処理にて保存不要データの消し　*
*                            込みを行う。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SZI0100B.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  更新累積ファイル  >>---*
     SELECT   ZAIEXLR2  ASSIGN    TO             ZAIEXLR2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   RUI-F01
                        FILE      STATUS    IS   RUI-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO             JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  更新累積ファイル  >>---*
 FD  ZAIEXLR2
                        LABEL     RECORD    IS   STANDARD.
                        COPY      ZAIEXLR2  OF   XFDLIB
                        JOINING   RUI       AS   PREFIX.
*---<<  条件ファイル   >>---*
 FD  HJYOKEN
                        LABEL     RECORD    IS   STANDARD.
                        COPY      HJYOKEN   OF   XFDLIB
                        JOINING   JYO       AS   PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  RUI-STATUS          PIC  X(02).
     03  JYO-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  READ-CNT            PIC  9(07)  VALUE ZERO.
     03  DEL-CNT             PIC  9(07)  VALUE ZERO.
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
 01  WK-RUI-F01              PIC  9(08)  VALUE ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SZI0100B".
       05  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE  " ###".
*-------------------------------------------------------------*
*       0.0   エラー処理                                      *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAIEXLR2.
     MOVE     "ZAIEXLR2"     TO   ERR-FL-ID.
     MOVE     RUI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
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
     PERFORM       MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM       END-SEC.
     STOP      RUN.
 CONTROL-END.
     EXIT.
*-------------------------------------------------------------*
*       2.0   初期処理                                        *
*-------------------------------------------------------------*
 INIT-SEC                    SECTION.
*ファイルのＯＰＥＮ
     OPEN    I-O             ZAIEXLR2.
     OPEN    INPUT           HJYOKEN.
*在庫締日取得
     MOVE    99           TO JYO-F01.
     MOVE   "ZAI"         TO JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY
         NC"＃＃　条件ファイル　取得エラー（在庫締め日）　＃＃"
                                          UPON CONS
             MOVE  4001   TO PROGRAM-STATUS
             STOP  RUN
     END-READ.
     MOVE    JYO-F05      TO WK-SIME.
*削除基準月数取得
     MOVE    83           TO JYO-F01.
     MOVE    "DATE6"      TO JYO-F02.
     READ    HJYOKEN  INVALID
             DISPLAY
         NC"＃＃　条件ファイル　取得エラー（削除基準日）　＃＃"
                                           UPON CONS
             MOVE  4001   TO PROGRAM-STATUS
             STOP  RUN
     END-READ.
*削除基準日算出
     MOVE    JYO-F04      TO WK-KIKAN.
     MOVE    WK-SIME(1:4) TO WRK-DATE1R1.
     MOVE    WK-SIME(5:2) TO WRK-DATE1R2.
     DIVIDE   WK-KIKAN    BY    12   GIVING    WK-CHK01
                                     REMAINDER WK-CHK02.
     COMPUTE DEL-DATE1-1  =  WRK-DATE1R1 - WK-CHK01.
     COMPUTE WK-TUKI      =  WRK-DATE1R2 - WK-CHK02.
     IF      WK-TUKI      <  ZERO
             ADD  -1      TO DEL-DATE1-1
             COMPUTE  WK-TUKI     = WK-TUKI * -1
             COMPUTE  DEL-DATE1-2 = 12 - WK-TUKI
     ELSE
             MOVE WK-TUKI TO DEL-DATE1-2
     END-IF.
     MOVE    99           TO DEL-DATE2.
*更新累積Ｆ読込み
     PERFORM RUI-READ-SEC.
     IF      "END"     =  END-FLG
             DISPLAY
             NC"＃＃　更新累積ファイル削除対象無　＃＃"
                                          UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
*-------------------------------------------------------------*
*      更新累積Ｆ読込み                                       *
*-------------------------------------------------------------*
 RUI-READ-SEC                SECTION.
*
     READ    ZAIEXLR2
             AT  END
             MOVE   "END"      TO   END-FLG
             GO                TO   RUI-READ-EXIT
             NOT AT END
             ADD     1         TO   READ-CNT
     END-READ.
*経過件数表示
     IF      READ-CNT(5:3)     =    "000"  OR  "500"
             DISPLAY "READ-CNT =    " READ-CNT UPON CONS
     END-IF.
*更新累積ファイルの作成日が削除基準日より大きい場合
     MOVE    RUI-F01           TO   WK-RUI-F01.
     IF      WK-RUI-F01  >  WK-DEL-DATE
             MOVE   "END"      TO   END-FLG
     END-IF.
*
 RUI-READ-EXIT.
     EXIT.
*-------------------------------------------------------------*
*      3.0　　メイン処理                                      *
*-------------------------------------------------------------*
 MAIN-SEC                    SECTION.
*更新累積Ｆ削除
     DELETE   ZAIEXLR2.
     ADD      1            TO     DEL-CNT.
*更新累積Ｆ読込み
     PERFORM RUI-READ-SEC.
*
 MAIN-END.
     EXIT.
*-------------------------------------------------------------*
*      4.0        終了処理                                    *
*-------------------------------------------------------------*
 END-SEC                SECTION.
     CLOSE              ZAIEXLR2  HJYOKEN.
     DISPLAY  NC"削除基準日　　　＝" WK-DEL-DATE UPON CONS.
     DISPLAY  NC"更新累積Ｆ　ＩＮ＝" READ-CNT    UPON CONS.
     DISPLAY  NC"更新累積Ｆ　削除＝" DEL-CNT     UPON CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
