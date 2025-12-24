# SSY5751B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5751B.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    サブシステム　　　　：　出荷管理システム
*    業務名　　　　　　　：　グッデイ出荷確定データ作成　　　　
*    モジュール名　　　　：　グッデイ出荷確定データ作成　　　　
*    作成日／更新日　　　：　10/07/12
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　売上伝票ファイルを読み、グッデイ　
*                        ：　出荷確定データを作成する。　　　　
*    更新履歴            ：
*      2011/10/07 飯田/NAV 基幹サーバ統合
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY5751B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/12.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                  *> 2011/10/07,S  S.I/NAV
                  *>     RECORD    KEY       DEN-F46
                  *>                         DEN-F47
                  *>                         DEN-F01
                  *>                         DEN-F48
                  *>                         DEN-F02
                  *>                         DEN-F04
                  *>                         DEN-F132
                  *>                         DEN-F03
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
                                           DEN-F03
                  *> 2011/10/07,E  S.I/NAV
                       FILE      STATUS    DEN-ST.
*発注集計プリントファイル
     SELECT   GOODAYKK  ASSIGN         GOODAYKK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         GKK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
                        COPY      SHTDENF  OF   XFDLIB
                        JOINING   DEN      AS   PREFIX.
****************************************************************
*    FILE = 発注集計プリントファイル                           *
****************************************************************
 FD  GOODAYKK           LABEL     RECORD   IS   STANDARD
                        BLOCK     CONTAINS 81   RECORDS.
 01  GKK-REC            PIC  X(50).
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  DEN-ST             PIC  X(02).
 01  GKK-ST             PIC  X(02).
*プログラムＩＤ
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "SSY5751B".
*日付ワーク
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG        PIC  X(03).
*ワーク領域
 01  WRK-AREA.
     03  WK-NONYU-DATE  PIC  9(08).
*エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*
*カウント領域
 01  CNT-AREA.
     03  CNT-READ            PIC  9(08).
     03  CNT-WRITE           PIC  9(08).
*出荷確定レコード
 01  WK-GKK-REC.
     03  WK-GKK-F01          PIC  9(09).
     03  WK-GKK-A01          PIC  X(01).
     03  WK-GKK-F02          PIC  9(02).
     03  WK-GKK-A02          PIC  X(01).
     03  WK-GKK-F03          PIC  9(05).
     03  WK-GKK-A03          PIC  X(01).
     03  WK-GKK-F04          PIC  9(07).
     03  FILLER              PIC  X(21).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-DATE           PIC  9(08).
 01  LINK-TIME           PIC  9(04).
 01  LINK-TORICD         PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-DATE  LINK-TIME
                                         LINK-TORICD.
****************************************************************
****************************************************************
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
****************************************************************
 DECLARATIVES.
*売上伝票データ
 DEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " SHTDENF ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*発注集計プリントファイル
 HAC-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      GOODAYKK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " GOODAYKK ERROR " GKK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
****************************************************************
 000-PROG-CNTL          SECTION.
     MOVE     "000-PROG-CNTL"     TO   S-NAME.
*開始メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  300-END-RTN.
******
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
****************************************************************
 100-INIT-RTN           SECTION.
     MOVE     "100-INIT-RTN"      TO   S-NAME.
*ファイルのオープン
     OPEN     INPUT     SHTDENF
     OPEN     OUTPUT    GOODAYKK.
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
*
*売上伝票データＲＥＡＤ
* 2011/10/07,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
* 2011/10/07,E  S.I/NAV
     MOVE     LINK-DATE           TO   DEN-F46
     MOVE     LINK-TIME           TO   DEN-F47
     MOVE     LINK-TORICD         TO   DEN-F01
     MOVE     SPACE               TO   DEN-F48
* 2011/10/07,S  S.I/NAV
**     START    SHTDENF   KEY       >=   DEN-F46
**                                       DEN-F47
**                                       DEN-F01
**                                       DEN-F48
**     END-START.
     START    SHTDENF   KEY       >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
     END-START.
* 2011/10/07,E  S.I/NAV
     PERFORM  900-DEN-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
****************************************************************
 200-MAIN-RTN           SECTION.
     MOVE     "200-MAIN-RTN"      TO   S-NAME.
*
*    発注集計表プリントファイル出力
     PERFORM  210-WRITE-RTN.
*    売上伝票データＲＥＡＤ
     PERFORM  900-DEN-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
****************************************************************
 300-END-RTN            SECTION.
     MOVE     "300-END-RTN"       TO   S-NAME.
*
*ファイルのクローズ
     CLOSE    SHTDENF
              GOODAYKK.
*
 300-END-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  3     発注集計表プリントファイル出力　             *
****************************************************************
 210-WRITE-RTN          SECTION.
     MOVE     "210-WRITE-RTN"     TO   S-NAME.
*
*レコード初期化
     MOVE     SPACE          TO   WK-GKK-REC  GKK-REC.
     INITIALIZE                   WK-GKK-REC  GKK-REC.
*    カンマセット
     MOVE     ","            TO   WK-GKK-A01  WK-GKK-A02
                                  WK-GKK-A03.
*    伝票番号
     MOVE     DEN-F02(4:6)   TO   WK-GKK-F01.
*    行番号
     MOVE     DEN-F03        TO   WK-GKK-F02.
*    店舗CD
     MOVE     DEN-F07        TO   WK-GKK-F03.
*    数量
     MOVE     DEN-F15        TO   WK-GKK-F04.
*    レコードセット
     MOVE     WK-GKK-REC     TO   GKK-REC.
*    出力
     WRITE    GKK-REC.
     ADD      1              TO   CNT-WRITE.
*
 210-WRITE-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    売上伝票データ　　 READ                      *
****************************************************************
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
*
     READ     SHTDENF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DEN-READ-EXIT
     END-READ.
*ブレイクチェック
     IF       DEN-F01   NOT =  LINK-TORICD      OR
              DEN-F47   NOT =  LINK-TIME        OR
              DEN-F46   NOT =  LINK-DATE
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DEN-READ-EXIT
     END-IF.
*
     ADD      1         TO        CNT-READ.
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
