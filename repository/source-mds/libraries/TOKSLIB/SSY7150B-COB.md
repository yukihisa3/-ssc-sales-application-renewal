# SSY7150B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY7150B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ジョイフル本田　仕入実績管理　　　*
*    業務名　　　　　　　：　受信仕入実績データ累積　　　　　　*
*    モジュール名　　　　：　受信仕入実績データ累積　　　　　　*
*    作成日／更新日　　　：　05/08/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受信した仕入実績データの累積を行 *
*                            なう。　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7150B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*変換伝票データ
     SELECT   JDJISKF   ASSIGN    TO        DA-01-VI-JDJISKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HEN-F01   HEN-F02
                                            HEN-F18   HEN-F27
                                            HEN-F08
                        FILE  STATUS   IS   HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1   RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01.
*        データ種区分
         05  DEN-01A             PIC  X(02).
*        レコード区分
         05  DEN-01B             PIC  X(02).
*
         05  DEN-01C             PIC  X(124).
******************************************************************
*    変換伝票データ　ＲＬ＝300
*****************************************************************
 FD  JDJISKF
                        BLOCK CONTAINS      13  RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     JDJISKF   OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-M1            PIC  9(08)     VALUE  ZERO.
 01  INV-FLG                 PIC  9(01)     VALUE  ZERO.
*
*ファイルヘッダレコード退避ワーク
 01  WK-DEPF-REC.
     03  WK-DEPF01          PIC  X(02).
     03  WK-DEPF02          PIC  X(02).
     03  WK-DEPF03          PIC  9(05).
     03  WK-DEPF04          PIC  9(08).
     03  WK-DEPF05          PIC  9(06).
     03  WK-DEPF06          PIC  9(08).
     03  WK-DEPF07          PIC  9(06).
     03  WK-DEPF08          PIC  9(05).
     03  WK-DEPF09          PIC  X(01).
     03  WK-DEPF10          PIC  X(05).
     03  WK-DEPF11          PIC  X(01).
     03  WK-DEPF12          PIC  X(79).
*ヘッダレコード退避ワーク
 01  WK-DEPH-REC.
     03  WK-DEPH01          PIC  X(02).
     03  WK-DEPH02          PIC  X(02).
     03  WK-DEPH03          PIC  9(05).
     03  WK-DEPH04          PIC  X(02).
     03  WK-DEPH05          PIC  X(05).
     03  WK-DEPH06          PIC  X(01).
     03  WK-DEPH07          PIC  X(02).
     03  WK-DEPH08          PIC  X(04).
     03  WK-DEPH09          PIC  X(10).
     03  WK-DEPH10          PIC  X(04).
     03  WK-DEPH11          PIC  X(04).
     03  WK-DEPH12          PIC  X(01).
     03  WK-DEPH13          PIC  X(07).
     03  WK-DEPH14          PIC  X(07).
     03  WK-DEPH15          PIC  S9(09)V9(02).
     03  WK-DEPH16          PIC  S9(09)V9(02).
     03  WK-DEPH17          PIC  9(08).
     03  WK-DEPH18          PIC  X(42).
*    明細レコード退避ワーク
 01  WK-DEPM-REC.
     03  WK-DEPM01          PIC  X(02).
     03  WK-DEPM02          PIC  X(02).
     03  WK-DEPM03          PIC  9(05).
     03  WK-DEPM04          PIC  9(02).
     03  WK-DEPM05          PIC  9(02).
     03  WK-DEPM06          PIC  X(01).
     03  WK-DEPM07          PIC  X(13).
     03  WK-DEPM08          PIC  S9(06)V9(02).
     03  WK-DEPM09          PIC  S9(08)V9(02).
     03  WK-DEPM10          PIC  S9(08)V9(02).
     03  WK-DEPM11          PIC  X(73).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7150B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7150B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7150B".
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
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE PARA-JTIME.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JDJISKF.
     MOVE      "JDJISKF"   TO    AB-FILE.
     MOVE      HEN-STATUS  TO    AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000        TO    PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
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
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       JDJISKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT   INV-FLG.
     MOVE     SPACE     TO        WK-DEPF-REC.
     INITIALIZE                   WK-DEPF-REC.
     MOVE     SPACE     TO        WK-DEPH-REC.
     INITIALIZE                   WK-DEPH-REC.
     MOVE     SPACE     TO        WK-DEPM-REC.
     INITIALIZE                   WK-DEPM-REC.
*
**************
*システム日付*
**************
     ACCEPT   SYS-DATE  FROM      DATE.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
              ADD       1             TO       RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*データ種区分が"03"（仕入実績）以外は読み飛ばし
     IF    DEN-01A   =     "03"
*ファイルヘッダ
         IF    DEN-01B   =     "F1"
               MOVE      SPACE         TO    WK-DEPF-REC
               INITIALIZE                    WK-DEPF-REC
               MOVE      DEN-01        TO    WK-DEPF-REC
*              送受信日
               MOVE     WK-DEPF06      TO       PARA-JDATE
*              送受信時分
               MOVE     WK-DEPF07(1:4) TO       PARA-JTIME
               DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS
               DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS
         END-IF
*
*仕入実績ヘッダ
         IF    DEN-01B    =    "H1"
               MOVE      SPACE       TO    WK-DEPH-REC
               INITIALIZE                  WK-DEPH-REC
               MOVE      DEN-01      TO    WK-DEPH-REC
               MOVE      ZERO        TO    CNT-KENSU-M1
          END-IF
*
*仕入実績明細
          IF   DEN-01B    =    "M1"
               MOVE      SPACE       TO    WK-DEPM-REC
               INITIALIZE                  WK-DEPM-REC
               MOVE      DEN-01      TO    WK-DEPM-REC
               ADD       1           TO    CNT-KENSU
               ADD       1           TO    CNT-KENSU-M1
*          変換データ 出力
               PERFORM  EDIT-SEC
          END-IF
     END-IF.

*
     READ   CVCSG001
         AT END
            MOVE     9           TO    END-FG
         NOT AT END
            ADD      1           TO    RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC.
     MOVE     ZERO          TO        INV-FLG.
*    レコード
*    ﾌｧｲﾙﾍｯﾀﾞ
*    送受信日
     MOVE     WK-DEPF06     TO        HEN-F01.
*    送受信時分
     MOVE     WK-DEPF07(1:4) TO       HEN-F02.
*    仕入先コード
     MOVE     WK-DEPF10     TO        HEN-F03.
*    メーカー識別コード
     MOVE     WK-DEPF11     TO        HEN-F04.
*    仕入実績ﾍｯﾀﾞ
*    データ種区分
     MOVE     WK-DEPH01     TO        HEN-F05.
*    レコード区分
     MOVE     WK-DEPH02     TO        HEN-F06.
*    レコードシーケンスN-.
     MOVE     WK-DEPH03     TO        HEN-F07.
*    訂正ＦＬＧ
     MOVE     WK-DEPH04     TO        HEN-F08.
*    仕入先コード
     MOVE     WK-DEPH05     TO        HEN-F09.
*    メーカー識別コード
     MOVE     WK-DEPH06     TO        HEN-F10.
*    伝票区分
     MOVE     WK-DEPH07     TO        HEN-F11.
*    店舗コード
     MOVE     WK-DEPH08     TO        HEN-F12.
*    店舗名称
     MOVE     WK-DEPH09     TO        HEN-F13.
*    部門コード
     MOVE     WK-DEPH10     TO        HEN-F14.
*    項目コード
     MOVE     WK-DEPH11     TO        HEN-F15.
*    特売区分
     MOVE     WK-DEPH12     TO        HEN-F16.
*    発注N-.
     MOVE     WK-DEPH13     TO        HEN-F17.
*    仕入伝票番号
     MOVE     WK-DEPH14     TO        HEN-F18.
*    原価計
     MOVE     WK-DEPH15     TO        HEN-F19.
*    値引高計
     MOVE     WK-DEPH16     TO        HEN-F20.
*    検収日
     MOVE     WK-DEPH17     TO        HEN-F21.
*    予備
     MOVE     WK-DEPH18     TO        HEN-F22.
*    仕入実績明細
*    データ種区分
     MOVE     WK-DEPM01     TO        HEN-F23.
*    レコード区分
     MOVE     WK-DEPM02     TO        HEN-F24.
*    レコードシーケンスN-.
     MOVE     WK-DEPM03     TO        HEN-F25.
*    発注行N-.
     MOVE     WK-DEPM04     TO        HEN-F26.
*    仕入行N-.
     MOVE     WK-DEPM05     TO        HEN-F27.
*    商品コード区分
     MOVE     WK-DEPM06     TO        HEN-F28.
*    商品コード
     MOVE     WK-DEPM07     TO        HEN-F29.
*    入荷数
     MOVE     WK-DEPM08     TO        HEN-F30.
*    原単価
     MOVE     WK-DEPM09     TO        HEN-F31.
*    値引高
     MOVE     WK-DEPM10     TO        HEN-F32.
*    予備
     MOVE     SPACE         TO        HEN-F33.
*    集計区分
     MOVE     ZERO          TO        HEN-F99.
*
*    存在チェック
     READ     JDJISKF       INVALID
              MOVE          "1"       TO       INV-FLG
     END-READ.
     IF       INV-FLG     =    1
              WRITE    HEN-REC
              ADD      1             TO   WRT-CNT
     END-IF.
*
 EDIT-EXIT.
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
     CLOSE     CVCSG001  JDJISKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
