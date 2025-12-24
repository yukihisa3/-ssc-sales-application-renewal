# CSV2010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/CSV2010B.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    業務名　　　　　　　：　出荷
*    サブシステム　　　　：　出荷管理システム
*    モジュール名　　　　：　新発注集計データ抽出
*    作成日／作成者　　　：　2022/05/11
*    処理概要　　　　　　：　売上伝票ファイルを読み、発注集計表
*                        ：　データを作成する。　　　
*    更新日／更新者　　　：　2022/05/19 INOUE
*    更新履歴　　　　　　：　発注集計表データへの転送項目追加
*    更新日／更新者　　　：　2023/03/31 INOUE
*    更新履歴　　　　　　：　発注集計表データへの転送項目追加
*    　　　　　　　　　　　　ルートＣＤ　　　　　　　　　　　　
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV2010B.
*                  流用:CSV0100B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/05/11.
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
                       FILE      STATUS    DEN-ST.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN         SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01  SHO-F02
                        STATUS         SHO-ST.
*SUB商品名称マスタ
     SELECT   SUBMEIF   ASSIGN         SUBMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SUB-F011  SUB-F0121
                                       SUB-F0122 SUB-F0123
                        STATUS         SUB-ST.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO   TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         TEN-ST.
*新発注集計データ
     SELECT   NHTMEIF   ASSIGN         NHTMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   DYNAMIC
                        RECORD    KEY  IS   HME-F01
                                            HME-F02
                                            HME-F03
                                            HME-F04
                                            HME-F05
                                            HME-F06
                                            HME-F15
                        STATUS         IS   HME-ST.
*新発注集計店舗データ
     SELECT   NHTTENF   ASSIGN         NHTTENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   DYNAMIC
                        RECORD    KEY  IS   HTE-F01
                        STATUS         IS   HTE-ST.
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
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
                        COPY      HSHOTBL  OF   XFDLIB
                        JOINING   SHO      AS   PREFIX.
****************************************************************
*    FILE = SUB商品名称マスタ　　　　　                        *
****************************************************************
 FD  SUBMEIF            LABEL     RECORD   IS   STANDARD.
                        COPY      SUBMEIF  OF   XFDLIB
                        JOINING   SUB      AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ　　　　　　　　                         *
****************************************************************
 FD  HTENMS             LABEL     RECORD   IS   STANDARD.
                        COPY      HTENMS   OF   XFDLIB
                        JOINING   TEN      AS   PREFIX.
****************************************************************
*    FILE = 新発注集計データ　　　　                           *
****************************************************************
 FD  NHTMEIF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTMEIF  OF   XFDLIB
                        JOINING   HME      AS   PREFIX.
****************************************************************
*    FILE = 新発注集計店舗データ　　                           *
****************************************************************
 FD  NHTTENF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTTENF  OF   XFDLIB
                        JOINING   HTE      AS   PREFIX.
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  SUB-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  HME-ST             PIC  X(02).
 01  HTE-ST             PIC  X(02).
*プログラムＩＤ
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "CSV2010B".
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
     03  END-FLG         PIC  X(03).
     03  HSHOTBL-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  SUBMEIF-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG  PIC  X(03)  VALUE  SPACE.
     03  NHTMEIF-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  NHTTENF-INV-FLG PIC  X(03)  VALUE  SPACE.
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
     03  CNT-READ            PIC  9(07).
     03  CNT-WRITE-HME       PIC  9(07).
     03  CNT-WRITE-HTE       PIC  9(05).
     03  CNT-SEQ             PIC  9(05).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  BATNO.
     03  LINK-DATE           PIC  9(08).
     03  LINK-TIME           PIC  9(04).
     03  LINK-TORICD         PIC  X(08).
 01  SOKOCD.
     03  LINK-SOKO-KAISI     PIC  X(02).
     03  LINK-SOKO-OWARI     PIC  X(02).
 01  LINK-IN-OUTKBN          PIC  X(01).
 01  LINK-OUT-WRITE-HME      PIC  9(07).
 01  LINK-OUT-WRITE-HTE      PIC  9(05).
 01  LINK-IN-OUTKBN2         PIC  X(01).
*
****************************************************************
 PROCEDURE              DIVISION  USING  BATNO
                                         SOKOCD
                                         LINK-IN-OUTKBN
                                         LINK-OUT-WRITE-HME
                                         LINK-OUT-WRITE-HTE
                                         LINK-IN-OUTKBN2.
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
*商品変換テーブル
 SHO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HSHOTBL ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*SUB商品名称マスタ
 SUB-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SUBMEIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " SUBMEIF ERROR " SUB-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*店舗マスタ
 TEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HTENMS  ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*新発注集計データ
 HME-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NHTMEIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " NHTMEIF ERROR " HME-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*新発注集計店舗データ
 HTE-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NHTTENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " NHTTENF ERROR " HTE-ST " "
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
     CLOSE    NHTTENF.
     OPEN     I-O NHTTENF.
     MOVE     SPACE          TO        END-FLG.
     PERFORM  210-MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  300-END-RTN.
******
*パラメタセット
     MOVE     CNT-WRITE-HME  TO   LINK-OUT-WRITE-HME.
     MOVE     CNT-WRITE-HTE  TO   LINK-OUT-WRITE-HTE.
*終了メッセージ出力
     DISPLAY  NC"　売上伝票　読込＝　" CNT-READ      UPON CONS.
     DISPLAY  NC"　発注データ抽出＝　" CNT-WRITE-HME UPON CONS.
     DISPLAY  NC"　店舗数　　　　＝　" CNT-WRITE-HTE UPON CONS.
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
                        HSHOTBL
                        SUBMEIF
                        HTENMS.
     OPEN     I-O       NHTMEIF
                        NHTTENF.
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
     DISPLAY "BATNO  = " BATNO  UPON CONS.
     DISPLAY "SOKOCD = " SOKOCD UPON CONS.
     DISPLAY "OUTKBN = " LINK-IN-OUTKBN UPON CONS.
*
*売上伝票データＲＥＡＤ
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE     LINK-DATE           TO   DEN-F46
     MOVE     LINK-TIME           TO   DEN-F47
     MOVE     LINK-TORICD         TO   DEN-F01
     MOVE     LINK-SOKO-KAISI     TO   DEN-F48
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
     INVALID
              MOVE     "END"      TO   END-FLG
              DISPLAY  NC"　＃対象データなし＃"  UPON CONS
              GO                  TO   100-INIT-RTN-EXIT
     END-START.
*
     PERFORM  900-DEN-READ.
     IF       END-FLG  =  "END"
              DISPLAY  NC"　＃対象データなし＃"  UPON CONS
              GO                  TO   100-INIT-RTN-EXIT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
****************************************************************
 200-MAIN-RTN           SECTION.
     MOVE     "200-MAIN-RTN"      TO   S-NAME.
*
*    ファイル出力
*      新発注集計データ
*      新発注集計店舗データ
     PERFORM  210-WRITE-RTN.
*
*    売上伝票データＲＥＡＤ
     PERFORM  900-DEN-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
****************************************************************
 210-MAIN-RTN           SECTION.
     MOVE     "210-MAIN-RTN"      TO   S-NAME.
*
*    店舗順番カウント  新発注集計店舗データ
     READ     NHTTENF
         NEXT AT END
              MOVE      "END"          TO   END-FLG
              GO                       TO   210-MAIN-RTN-EXIT
     END-READ.
     ADD      1                   TO   CNT-SEQ.
     MOVE     CNT-SEQ             TO   HTE-F03.
     REWRITE  HTE-REC.
*
 210-MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
****************************************************************
 300-END-RTN            SECTION.
     MOVE     "300-END-RTN"       TO   S-NAME.
*
*ファイルのクローズ
     CLOSE    SHTDENF
              HSHOTBL
              SUBMEIF
              HTENMS
              NHTMEIF
              NHTTENF.
*
 300-END-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  3     ファイル出力　　　　　　　　　　             *
*                     新発注集計データ　　　　　　             *
*                     新発注集計店舗データ　　　　             *
****************************************************************
 210-WRITE-RTN          SECTION.
     MOVE     "210-WRITE-RTN"     TO   S-NAME.
*
 210-WRITE-RTN01.
*新発注集計データ
*    存在チェック
     MOVE     LINK-DATE      TO   HME-F01.
     MOVE     LINK-TIME      TO   HME-F02.
     MOVE     LINK-TORICD    TO   HME-F03.
     MOVE     DEN-F48        TO   HME-F04.
     MOVE     DEN-F112       TO   HME-F05.
     MOVE     DEN-F25        TO   HME-F06.
     MOVE     DEN-F07        TO   HME-F15.
     PERFORM  930-HME-READ.
     IF       NHTMEIF-INV-FLG  =  "INV"
              CONTINUE
     ELSE
              GO             TO   210-WRITE-RTN03
     END-IF.
*
 210-WRITE-RTN02.
*作成
*    レコード初期化
     MOVE     SPACE          TO   HME-REC.
     INITIALIZE                   HME-REC.
*    バッチ（日）
     MOVE     LINK-DATE      TO   HME-F01.
*    バッチ（時）
     MOVE     LINK-TIME      TO   HME-F02.
*    バッチ（取）
     MOVE     LINK-TORICD    TO   HME-F03.
*    倉庫コード
     MOVE     DEN-F48        TO   HME-F04.
*    納入日
     MOVE     DEN-F112       TO   HME-F05.
*    量販店商品コード
     MOVE     DEN-F25        TO   HME-F06.
*    サカタコード
***  商品変換テーブルＲＥＡＤ（売上伝票Ｆに自社商品コード無時）
     IF       DEN-F1411      =    SPACE
              MOVE     LINK-TORICD    TO   SHO-F01
              MOVE     DEN-F25        TO   SHO-F02
              PERFORM  910-SHO-READ
              MOVE     SHO-F031       TO   HME-F07
              MOVE     SHO-F032       TO   HME-F08
*** （売上伝票Ｆに自社商品コード有時）
     ELSE
              MOVE     DEN-F1411      TO   HME-F07
              MOVE     DEN-F1412      TO   HME-F08
     END-IF.
*    商品名
     MOVE     DEN-F1421      TO   HME-F121.
     MOVE     DEN-F1422      TO   HME-F122.
*    原単価
     MOVE     DEN-F172       TO   HME-F13.
*    売単価
     MOVE     DEN-F173       TO   HME-F14.
*    店舗コード
     MOVE     DEN-F07        TO   HME-F15.
*    受注時数量
     MOVE     DEN-F50        TO   HME-F17.
*    訂正後数量
     MOVE     DEN-F15        TO   HME-F18.
*    商品名漢字
     MOVE     DEN-F1411      TO   SUB-F011.
     MOVE     DEN-F1412      TO   SUB-F012.
     PERFORM  920-SUB-READ.
     IF  SUBMEIF-INV-FLG  =  "INV"
         MOVE ALL NC"？"     TO   HME-F19
                                  HME-F20
     ELSE
         MOVE SUB-F021       TO   HME-F19
         MOVE SUB-F022       TO   HME-F20
     END-IF.
*    在庫引当区分
     MOVE     DEN-F27D       TO   HME-F21.
     IF       DEN-F15  =  ZERO
              MOVE  1        TO   HME-F21
     END-IF.
*#2022/08/22 NAV ST
*ＰＣ側が出力分が逆の判断になっているのでホスト側で変更する。
     IF       HME-F21  =  "0"
              MOVE "1"       TO   HME-F21
     ELSE
              MOVE "0"       TO   HME-F21
     END-IF.
*#2022/08/22 NAV ED
*    ＯＲＤ区分（小売連携区分）
     MOVE     DEN-F32        TO   HME-F22.
*    ＳＴＮＯ管理
     IF  SUBMEIF-INV-FLG  =  "INV"

         MOVE SPACE          TO   HME-F23
     ELSE
         IF   SUB-F97 = 1
              MOVE   "1"     TO   HME-F23
         ELSE
              MOVE   SPACE   TO   HME-F23
         END-IF
     END-IF.
*    出力区分
*#2023/07/07 NAV ST
*****MOVE     LINK-IN-OUTKBN TO   HME-F24.
     IF  LINK-IN-OUTKBN  =  "1"
         MOVE ""             TO   HME-F24
     ELSE
         MOVE "1"            TO   HME-F24
     END-IF.
*#2023/07/07 NAV ED
*↓2022/05/19
*    商品カテゴリ
     IF  SUBMEIF-INV-FLG  =  "INV"

         MOVE SPACE          TO   HME-F30
     ELSE
         MOVE SUB-F09        TO   HME-F30
     END-IF.
*    Ｄ３６５用ＪＡＮＣＤ
     IF  SUBMEIF-INV-FLG  =  "INV"

         MOVE SPACE          TO   HME-F31
     ELSE
         MOVE SUB-D01        TO   HME-F31
     END-IF.
*↑2022/05/19
*↓2023/03/31
*    ルートＣＤ ＫＢＮ２が１の時は印字しない
     IF  LINK-IN-OUTKBN2  =  "1"
         MOVE   SPACE        TO   HME-F25
     ELSE
         MOVE   DEN-F42      TO   HME-F25
     END-IF.
*↑2023/03/31
*
*    出力
     WRITE    HME-REC.
     ADD      1              TO   CNT-WRITE-HME.
*
*新発注集計店舗データ
*    存在チェック
     MOVE     DEN-F07        TO   HTE-F01.
     PERFORM  930-HTE-READ.
     IF  NHTTENF-INV-FLG  =  "INV"
*        レコード初期化
         MOVE     SPACE          TO   HTE-REC
         INITIALIZE                   HTE-REC
*        店舗ＣＤ
         MOVE     DEN-F07        TO   HTE-F01
*
         MOVE     LINK-TORICD    TO   TEN-F52
         MOVE     DEN-F07        TO   TEN-F011
         PERFORM  940-TEN-READ
         IF       HTENMS-INV-FLG  =  "INV"
*                 店舗名
                  MOVE ALL NC"？"     TO   HTE-F02
         ELSE
*                 店舗名
                  MOVE TEN-F03        TO   HTE-F02
         END-IF
*        出力
         WRITE    HTE-REC
         ADD      1              TO   CNT-WRITE-HTE
     END-IF.
     GO                          TO   210-WRITE-RTN-EXIT.
*
 210-WRITE-RTN03.
*更新
*    受注時数量
     ADD      DEN-F50        TO   HME-F17.
*    訂正後数量
     ADD      DEN-F15        TO   HME-F18.
*
     REWRITE  HME-REC.
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
     IF       DEN-F48   >      LINK-SOKO-OWARI  OR
              DEN-F01   NOT =  LINK-TORICD      OR
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
****************************************************************
*    LEVEL ALL    商品コード変換テーブル　READ                 *
****************************************************************
 910-SHO-READ           SECTION.
     MOVE     "910-SHO-READ"      TO   S-NAME.
*
     READ     HSHOTBL
         INVALID
         DISPLAY "商品テーブルエラー　取引先　＝　" SHO-F01
               "　店舗　＝　"  DEN-F07
               "　商品　＝　"  SHO-F02
              MOVE      SPACE     TO   SHO-F031 SHO-F032
*             MOVE      ZERO      TO   SHO-F04
     END-READ.
*
 910-SHO-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    SUB商品名称マスタ読込　　　　　              *
****************************************************************
 920-SUB-READ           SECTION.
     MOVE     "920-SUB-READ"      TO   S-NAME.
*
     READ     SUBMEIF
         INVALID
         MOVE      "INV"          TO   SUBMEIF-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   SUBMEIF-INV-FLG
     END-READ.
*
 920-SUB-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計データ検索　　　　　               *
****************************************************************
 930-HME-READ           SECTION.
     MOVE     "930-HME-READ"      TO   S-NAME.
*
     READ     NHTMEIF
         INVALID
         MOVE      "INV"          TO   NHTMEIF-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   NHTMEIF-INV-FLG
     END-READ.
*
 930-HME-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計店舗データ検索　　　               *
****************************************************************
 930-HTE-READ           SECTION.
     MOVE     "930-HTE-READ"      TO   S-NAME.
*
     READ     NHTTENF
         INVALID
         MOVE      "INV"          TO   NHTTENF-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   NHTTENF-INV-FLG
     END-READ.
*
 930-HTE-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    店舗マスタ検索　　　　　　　　               *
****************************************************************
 940-TEN-READ           SECTION.
     MOVE     "940-TEN-READ"      TO   S-NAME.
*
     READ     HTENMS
         INVALID
         MOVE      "INV"          TO   HTENMS-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   HTENMS-INV-FLG
     END-READ.
*
 940-TEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
