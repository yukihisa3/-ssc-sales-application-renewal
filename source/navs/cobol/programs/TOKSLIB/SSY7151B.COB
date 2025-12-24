****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ作成　　　　　　　*
*    作成日／更新日　　　：　05/08/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*　　　　　　　　　　　　　　ジョイフル本田　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7151B.
 AUTHOR.                C FUJIWARA.
 DATE-WRITTEN.          05/05/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*仕入実績データファイル
     SELECT   JDJISKF   ASSIGN    TO        DA-01-VI-JDJISKL2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        RECORD    KEY       JIS-F99   JIS-F01
                                            JIS-F02   JIS-F18
                                            JIS-F27   JIS-F08
                        ORGANIZATION   IS   INDEXED.
*集計データ
     SELECT   JDSYUKF   ASSIGN    TO        DA-01-VI-JDSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY                 SYU-F04
                                            SYU-F02   SYU-F03
                        FILE  STATUS   IS   SYU-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    読込データ　ＲＬ＝　３００　  ＢＦ＝　１
******************************************************************
 FD  JDJISKF
                        BLOCK CONTAINS      13   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     JDJISKSF  OF        XFDLIB
              JOINING   JIS  AS   PREFIX.
******************************************************************
*    実績集計データ　ＲＬ＝１００
******************************************************************
 FD  JDSYUKF
                        LABEL RECORD   IS   STANDARD.
     COPY     JDSYUKF   OF        XFDLIB
              JOINING   SYU  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WR-FLG                  PIC  9.
*
 01  WK-JSKF-REC.
*    入荷数
     03  WK-NYUKAS          PIC  S9(06)V9(02).
*    原価金額
     03  WK-GENKIN          PIC  S9(10).
*ブレイクキー
 01  BREAK-KEY.
     03  NEW-VALUE.
         05  NEW-TEN        PIC  X(04).
         05  NEW-DEN        PIC  9(07).
         05  NEW-DNO        PIC  9(02).
     03  OLD-VALUE.
         05  OLD-TEN        PIC  X(04).
         05  OLD-DEN        PIC  9(07).
         05  OLD-DNO        PIC  9(02).
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC  9(06).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SYU-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7151B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7151B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7151B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JDJISKF.
     MOVE      "JDJISKF"    TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JDSYUKF.
     MOVE      "JDSYUKF"    TO   AB-FILE.
     MOVE      SYU-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       JDJISKF JDSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-JSKF-REC.
     INITIALIZE                   WK-JSKF-REC.

     MOVE     LOW-VALUE      TO   BREAK-KEY.
 INIT-040.
*
**************
*システム日付*
**************
     ACCEPT   SYS-DATE  FROM      DATE.
*
     PERFORM  JDJISKF-READ-SEC.
*
     MOVE     NEW-VALUE      TO   OLD-VALUE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*存在チェック
     MOVE    JIS-F12   TO  SYU-F04
     MOVE    JIS-F18   TO  SYU-F02
     MOVE    JIS-F27   TO  SYU-F03
     READ    JDSYUKF   INVALID
                       MOVE   0  TO  WR-FLG
             NOT INVALID
                       MOVE   1  TO  WR-FLG
     END-READ.
*
     PERFORM  SYUKEI-SEC.
*
     PERFORM  JDJISKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル集計　　　　　　　　　　　　　　　　　　*
****************************************************************
 SYUKEI-SEC              SECTION.
*
     MOVE    "SYUKEI-SEC"   TO        S-NAME.
     MOVE     SPACE         TO        SYU-REC.
     INITIALIZE                       SYU-REC.
*    検収日
     MOVE     JIS-F21       TO        SYU-F01.
*    伝票ＮＯ
     MOVE     JIS-F18       TO        SYU-F02.
*    行ＮＯ
     MOVE     JIS-F27       TO        SYU-F03.
*    店舗ＣＤ
     MOVE     JIS-F12       TO        SYU-F04.
*    店舗名称
     MOVE     JIS-F13       TO        SYU-F05.
*    伝票区分
     MOVE     JIS-F11       TO        SYU-F06.
*    商品ＣＤ
     MOVE     JIS-F29       TO        SYU-F07.
*    入荷数合計
     COMPUTE  SYU-F08  =  SYU-F08  + JIS-F30.
*    原単価
     MOVE     JIS-F31       TO        SYU-F09.
*    原価金額(入荷数×原単価)
     COMPUTE  SYU-F10  =  JIS-F30 * JIS-F31.
*    部門ＣＤ
     MOVE     JIS-F14(3:2)  TO        SYU-F11.
*    項目ＣＤ
     MOVE     JIS-F15(3:2)  TO        SYU-F12.
*    区分ＣＤ
     MOVE     JIS-F16       TO        SYU-F13.
*    予備TODO
     MOVE     SPACE         TO        SYU-F98.
*    請求区分TODO
     MOVE     SPACE         TO        SYU-F99.
     IF   WR-FLG =  ZERO
          WRITE   SYU-REC
     ELSE
          REWRITE SYU-REC
     END-IF.
*    集計区分に１を立てる
     MOVE     "1"           TO        JIS-F99.
*
     REWRITE    JIS-REC.
     ADD      1             TO        WRT-CNT.
     MOVE   ZERO            TO        WK-NYUKAS
                                      WK-GENKIN.
*
 SYUKEI-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     JDJISKF  JDSYUKF.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読み込み
****************************************************************
 JDJISKF-READ-SEC   SECTION.
*
     MOVE     "JDJISKF-READ-SEC"  TO  S-NAME.
*
     READ     JDJISKF
              AT END    MOVE      9         TO  END-FG
                        GO                  TO  JDJISKF-READ-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*仕入実績集計(集計区分＝１以外が対象)
     IF   JIS-F99   NOT =     "1"
          CONTINUE
     ELSE
          MOVE      "9"        TO         END-FG
     END-IF.
*
 JDJISKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
