# SSY1237B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY1237B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　取引先修正情報　作成（送信用）　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/02/01 INOUE                  *
*    処理概要　　　　　　：　取引先への送信データを作成する。　*
*                            （取引先修正情報：一括・ケース）　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1237B.
*                  流用:SSY1225B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/02/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 取引先修正情報抽出データ >>--*
     SELECT   KNSNDCGW  ASSIGN    TO        DA-01-VS-KNSNDCGW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   IN-STATUS.
*----<< 取引先修正情報送信データ >>----*
     SELECT   KNSNDCG   ASSIGN    TO        DA-01-S-KNSNDCG
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    OUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    取引先修正情報抽出データ
******************************************************************
 FD  KNSNDCGW           LABEL     RECORD   IS   STANDARD.
     COPY     KNSNDCGW  OF        XFDLIB
              JOINING   IN        PREFIX.
******************************************************************
*    取引先修正情報送信データ
******************************************************************
 FD  KNSNDCG            BLOCK     CONTAINS  1   RECORDS.
 01  OUT-REC.
     03  FILLER         PIC  X(64).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*    取引先修正情報送信データＣＯＰＹ句
     COPY     KNSNDCG1  OF        XFDLIB
              JOINING   CG1  AS   PREFIX.
     COPY     KNSNDCG2  OF        XFDLIB
              JOINING   CG2  AS   PREFIX.
*
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DATA-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNSNDCG-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  IX                      PIC  9(05)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
*ブレイク項目
*01  BRK-F001              PIC  9(08)   VALUE ZERO.
*01  BRK-F002              PIC  9(04)   VALUE ZERO.
*01  BRK-F003              PIC  9(08)   VALUE ZERO.
*01  BRK-F004              PIC  X(02)   VALUE SPACE.
*01  BRK-FA05              PIC  9(06)   VALUE ZERO.
*01  BRK-FA01              PIC  9(04)   VALUE ZERO.
 01  BRK-FA07              PIC  9(04)   VALUE ZERO.
 01  BRK-FA08              PIC  9(02)   VALUE ZERO.
*01  BRK-FA06              PIC  9(02)   VALUE ZERO.
*01  BRK-FA04              PIC  9(06)   VALUE ZERO.
*
 01  WK-ST.
     03  IN-STATUS         PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1237B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1237B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1237B".
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
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-IN-BUMON             PIC   X(04).
*01  PARA-IN-TANTOU            PIC   X(02).
*01  PARA-IN-JDATE             PIC   9(08).
*01  PARA-IN-JTIME             PIC   9(04).
*01  PARA-IN-TORICD            PIC   9(08).
*01  PARA-IN-SOKO              PIC   X(02).
*01  PARA-IN-STEN              PIC   9(05).
*01  PARA-IN-ETEN              PIC   9(05).
*01  PARA-IN-SROUTE            PIC   9(02).
*01  PARA-IN-EROUTE            PIC   9(02).
*01  PARA-IN-SBUMON            PIC   9(02).
*01  PARA-IN-EBUMON            PIC   9(02).
*01  PARA-IN-HDATE             PIC   9(08).
*01  PARA-IN-NDATE             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*PROCEDURE              DIVISION USING
*                                 PARA-IN-BUMON
*                                 PARA-IN-TANTOU
*                                 PARA-IN-JDATE
*                                 PARA-IN-JTIME
*                                 PARA-IN-TORICD
*                                 PARA-IN-SOKO
*                                 PARA-IN-STEN
*                                 PARA-IN-ETEN
*                                 PARA-IN-SROUTE
*                                 PARA-IN-EROUTE
*                                 PARA-IN-SBUMON
*                                 PARA-IN-EBUMON
*                                 PARA-IN-HDATE
*                                 PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNSNDCGW.
     MOVE      "KNSNDCGW "   TO   AB-FILE.
     MOVE      IN-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNSNDCG.
     MOVE      "KNSNDCG"    TO   AB-FILE.
     MOVE      OUT-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KNSNDCGW.
     OPEN     OUTPUT    KNSNDCG.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    件数初期セット
     MOVE    1                      TO  DATA-CNT.
*
*    取引先修正情報抽出データ読込み
     PERFORM KNSNDCGW-READ-SEC.
     IF      END-FLG  NOT = "END"
             MOVE     IN-FA07       TO  BRK-FA07
             MOVE     IN-FA08       TO  BRK-FA08
     ELSE
             MOVE     4010          TO  PROGRAM-STATUS
             STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNSNDCGW-READ-SEC    SECTION.
*
     MOVE    "KNSNDCGW-READ-SEC"    TO  S-NAME.
*
     READ     KNSNDCGW
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNSNDCGW-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNSNDCGW-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*データ件数カウントアップ
 MAIN000.
     ADD     1                       TO  DATA-CNT.
*
*明細レコード編集・出力
 MAIN001.
     PERFORM  MEISAI-OUT-SEC.
*
*取引先修正情報抽出データ読込み
 MAIN888.
     PERFORM  KNSNDCGW-READ-SEC.
     IF       END-FLG  =  "END"
*             ヘッダレコード編集・出力
              PERFORM  HEAD-OUT-SEC
              GO       TO   MAIN-EXIT
     END-IF.
*
*ブレイク判定（取引先・ルート）
 MAIN999.
     IF    ( IN-FA07   =  BRK-FA07 ) AND
           ( IN-FA08   =  BRK-FA08 )
             CONTINUE
     ELSE
*            ヘッダレコード編集・出力
             PERFORM   HEAD-OUT-SEC
             MOVE      1             TO  DATA-CNT
*            ブレイクキー入替
             MOVE      IN-FA07       TO  BRK-FA07
             MOVE      IN-FA08       TO  BRK-FA08
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　取引先修正情報送信データ出力処理（明細）
****************************************************************
 MEISAI-OUT-SEC  SECTION.
*
     MOVE     "MEISAI-OUT-SEC"     TO      S-NAME.
*
     MOVE      SPACE               TO      CG2-REC.
     INITIALIZE                            CG2-REC.
*取引先コード
     MOVE      IN-FA07             TO      CG2-F01.
*ルートコード
     MOVE      IN-FA08             TO      CG2-F02.
*ファイル種別
     MOVE      2                   TO      CG2-F03.
*レコード区分
     MOVE      1                   TO      CG2-F04.
*レコード連番
     MOVE      DATA-CNT            TO      CG2-F05.
*伝票番号ド
     MOVE      IN-FA06             TO      CG2-F06.
*ＪＡＮコード
     MOVE      IN-FB11             TO      CG2-F07.
*ケースフラグ
     MOVE      IN-FB12             TO      CG2-F08.
*発注数量
     MOVE      IN-FB08             TO      CG2-F09.
*修正後数量
     MOVE      IN-FD01             TO      CG2-F10.
*メーカー直納情報
*予備
*契約番号
*契約枝番
*
     WRITE     OUT-REC             FROM    CG2-REC.
     ADD       1                   TO      WRT-CNT.
*
 MEISAI-OUT-EXIT.
     EXIT.
****************************************************************
*　　取引先修正情報送信データ出力処理（ヘッダ）
****************************************************************
 HEAD-OUT-SEC  SECTION.
*
     MOVE     "HEAD-OUT-SEC"       TO      S-NAME.
*
     MOVE      SPACE               TO      CG1-REC.
     INITIALIZE                            CG1-REC.
*取引先コード
     MOVE      BRK-FA07            TO      CG1-F01.
*ルートコード
     MOVE      BRK-FA08            TO      CG1-F02.
*ファイル種別
     MOVE      2                   TO      CG1-F03.
*レコード区分
     MOVE      0                   TO      CG1-F04.
*レコード連番
     MOVE      1                   TO      CG1-F05.
*送信区分
     MOVE      1                   TO      CG1-F06.
*データ件数
     MOVE      DATA-CNT            TO      CG1-F07.
*データ作成日
     MOVE      SYS-DATEW           TO      CG1-F08.
*データ作成時間
     MOVE      WK-TIME(1:6)        TO      CG1-F09.
*
     WRITE     OUT-REC             FROM    CG1-REC.
     ADD       1                   TO      WRT-CNT.
*
 HEAD-OUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"修正ファイル"   "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"送信ファイル"   "OUT = " WRT-CNT   UPON CONS.
*
     IF        WRT-CNT  =  ZERO
               MOVE    4010    TO    PROGRAM-STATUS
               STOP    RUN
     END-IF.
*
     CLOSE     KNSNDCGW  KNSNDCG.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
