# SBM1004B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM1004B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳオンライン　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    モジュール名　　　　：　イオン取引先ＣＤ付替処理（支払）　*
*    作成日／更新日　　　：　2012/11/13                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、受領Ｍ  *
*                            ＳＧを読み、マスタを参照して取引  *
*                            先ＣＤを取得して、取引先ＣＤの付　*
*                            替を行なう。　　　　　　　　　　　*
*    データ区分＝１：発注ＭＳＧ、　　２：受領ＭＳＧ　　　　　　*
*                ３：受領訂正ＭＳＧ、４：返品ＭＳＧ　　　　　　*
*                ５：支払ＭＳＧ　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　2013/01/31                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　会社ＣＤの項目を、発注者ＣＤに変　*
*                          　更する。　　　　　　　　　　　　  *
*    作成日／更新日　　　：　2013/02/28                        *
*    作成者／更新者　　　：　ＮＡＶ井上　　　　　　　　　　　　*
*    処理概要　　　　　　：　サンデー・ジョイ追加　　　　　　　*
*                          　　振分マスタ非存在時の固定値セット*
*    作成日／更新日　　　：　2015/05/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗ＣＤ４桁→５桁対応　　　　　  *
*    作成日／更新日　　　：　2015/11/17                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　イオンリテールストア　　　　　　  *
*                            （旧ダイエー）                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM1004B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/13.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ支払メッセージ
     SELECT   BMSSIHF   ASSIGN    TO        DA-01-VI-BMSSIHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SIH-F011  SIH-F012
                                            SIH-F013
                                            WITH DUPLICATES
                        FILE      STATUS    IS   SIH-ST.
*イオン取引先振分マスタ
     SELECT   IONFURF   ASSIGN    TO        DA-01-VI-IONFURL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   FUR-F01
                                                 FUR-F02
                        FILE      STATUS    IS   FUR-ST.
*イオン取引先受信ワーク
     SELECT   IONTWKF   ASSIGN    TO        DA-01-VI-IONTWKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TWK-F01
                                            TWK-F02
                                            TWK-F03
                                            TWK-F04
                        FILE      STATUS    IS   TWK-ST.
*イオン取引先付替エラーワーク
     SELECT   IONTERWK  ASSIGN    TO        DA-01-S-IONTERWK
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   TER-ST.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    流通ＢＭＳ支払メッセージ
******************************************************************
 FD  BMSSIHF
                        LABEL RECORD   IS   STANDARD.
     COPY     BMSSIHF   OF        XFDLIB
              JOINING   SIH  AS   PREFIX.
*
******************************************************************
*    イオン取引先振分マスタ
******************************************************************
 FD  IONFURF            LABEL RECORD   IS   STANDARD.
     COPY     IONFURF   OF        XFDLIB
              JOINING   FUR       PREFIX.
*
******************************************************************
*    イオン取引先受信ワーク
******************************************************************
 FD  IONTWKF            LABEL RECORD   IS   STANDARD.
     COPY     IONTWKF   OF        XFDLIB
              JOINING   TWK       PREFIX.
*
******************************************************************
*    イオン取引先付替エラーワーク
******************************************************************
 FD  IONTERWK           BLOCK     CONTAINS  81  RECORDS
                        LABEL     RECORD    IS  STANDARD.
     COPY     IONTERWK  OF        XFDLIB
              JOINING   TER       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワークに支払メッセージを展開
     COPY   BMSSIHF  OF XFDLIB  JOINING   HWK  AS   PREFIX.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  READ-CNT                PIC  9(08)     VALUE  ZERO.
 01  HENK-CNT                PIC  9(08)     VALUE  ZERO.
 01  ERRT-CNT                PIC  9(08)     VALUE  ZERO.
 01  IONTWKF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  IONFURF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  SIH-ST        PIC  X(02).
     03  FUR-ST        PIC  X(02).
     03  TWK-ST        PIC  X(02).
     03  TER-ST        PIC  X(02).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** 店舗ＣＤ変換
 01  WK-TENPO           PIC  X(04).
 01  FILLER             REDEFINES      WK-TENPO.
     03  WK-TENPO-H     PIC  9(04).
*##2015/05/28 NAV ST 店舗ＣＤ５桁対応
***** 店舗ＣＤ変換
 01  WK-TENPO5          PIC  X(05).
 01  FILLER             REDEFINES      WK-TENPO5.
     03  WK-TENPO5-H    PIC  9(05).
*##2015/05/28 NAV ED
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  HAC-ERR           PIC  N(20)  VALUE
         NC"流通ＢＭＳ支払メッセージエラー".
     03  FUR-ERR           PIC  N(20)  VALUE
         NC"イオン取引先振分マスタエラー".
     03  TWK-ERR           PIC  N(20)  VALUE
         NC"イオン取引先受信ワークエラー".
     03  TER-ERR           PIC  N(20)  VALUE
         NC"イオン取引先付替エラーワークエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBM1004B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM1004B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-ERR.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " ERRCNT = ".
         05  ERR-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ定義
 LINKAGE                SECTION.
 01  PARA-HIDUKE            PIC   9(08).
 01  PARA-JIKAN             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-HIDUKE
                            PARA-JIKAN
                            PARA-TORICD.
 DECLARATIVES.
 HAC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BMSSIHF.
     DISPLAY     HAC-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SIH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FUR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONFURF.
     DISPLAY     FUR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FUR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONTWKF.
     DISPLAY     TWK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TWK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TER-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONTERWK.
     DISPLAY     TER-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TER-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       BMSSIHF.
     OPEN     INPUT     IONFURF.
     OPEN     I-O       IONTWKF.
     OPEN     EXTEND    IONTERWK.
*
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
*流通ＢＭＳ支払メッセージスタート
     MOVE     SPACE          TO   SIH-REC.
     INITIALIZE                   SIH-REC.
     MOVE     PARA-HIDUKE    TO   SIH-F011.
     MOVE     PARA-JIKAN     TO   SIH-F012.
     MOVE     PARA-TORICD    TO   SIH-F013.
     START    BMSSIHF  KEY  >=    SIH-F011  SIH-F012  SIH-F013
         INVALID   KEY
              DISPLAY NC"＃付替対象取引先がありません！！１"
                      UPON CONS
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*流通ＢＭＳ支払メッセージ読込
     PERFORM  BMSSIHF-READ-SEC.
     IF       END-FLG = "END"
              DISPLAY NC"＃付替対象取引先がありません！！１"
                      UPON CONS
              GO             TO   INIT-EXIT
     END-IF.
*
 INIT-010.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*流通ＢＭＳ支払メッセージをワークに退避する。
     MOVE     SIH-REC             TO   HWK-REC.
*取引先ＣＤを取得する。
     PERFORM  IONFURF-READ-SEC.
     IF  IONFURF-INV-FLG  =  "INV"
*********イオン取引先振分マスタが存在しない場合
*********バッチ番号＋会社ＣＤ＋店舗ＣＤをワークに出力する。
*********会社ＣＤで取引先ＣＤを判定する。
         MOVE     SPACE           TO   TER-REC
         INITIALIZE                    TER-REC
         MOVE     PARA-HIDUKE     TO   TER-F01
         MOVE     PARA-JIKAN      TO   TER-F02
         MOVE     PARA-TORICD     TO   TER-F03
*2013/01/31 NAV ST 支払法人ＣＤ→発注者ＣＤに変更
*********MOVE     SIH-F206        TO   TER-F04
         MOVE     SIH-F210        TO   TER-F04
*2013/01/31 NAV ED 支払法人ＣＤ→発注者ＣＤに変更
******** MOVE     SIH-F405(1:4)   TO   WK-TENPO
******** MOVE     WK-TENPO-H      TO   TER-F05
*2013/01/31 NAV ST 支払法人ＣＤ→発注者ＣＤに変更
*##2015/05/28 NAV ST 店舗ＣＤ５桁対応
******** MOVE     SIH-F405(1:4)   TO   WK-TENPO
******** MOVE     WK-TENPO-H      TO   TER-F05
         IF  SIH-F405(5:1) = SPACE
             MOVE SIH-F405(1:4)   TO   WK-TENPO
             MOVE WK-TENPO-H      TO   TER-F05
         ELSE
             MOVE SIH-F405(1:5)   TO   WK-TENPO5
             MOVE WK-TENPO5-H     TO   TER-F05
         END-IF
*##2015/05/28 NAV ED 店舗ＣＤ５桁対応
*********EVALUATE SIH-F206(1:4)
         EVALUATE SIH-F210(1:4)
*2013/01/31 NAV ED 支払法人ＣＤ→発注者ＣＤに変更
             WHEN "5200"
*                イオン北海道
                   MOVE 87372    TO   HWK-F013 SIH-F013 TER-F06
             WHEN "0105"
*                イオンリテール
                   MOVE 87373    TO   HWK-F013 SIH-F013 TER-F06
             WHEN "5300"
*                イオンスーパーセンター
                   MOVE 87374    TO   HWK-F013 SIH-F013 TER-F06
             WHEN "3618"
*                イオン九州
                   MOVE 33189    TO   HWK-F013 SIH-F013 TER-F06
             WHEN "2400"
*                イオンビッグ
                   MOVE 87373    TO   HWK-F013 SIH-F013 TER-F06
*       2013/02/28↓追加
             WHEN "8200"
*                サンデー
                   MOVE 30402    TO   HWK-F013 SIH-F013 TER-F06
             WHEN "8000"
*                ジョイ
                   MOVE 304021   TO   HWK-F013 SIH-F013 TER-F06
*       2013/02/28↑追加
*       2015/11/17↓追加
             WHEN "7900"
*                 イオンリテールストア
                   MOVE 87376    TO   HWK-F013 SIH-F013 TER-F06
*       2015/11/17↑追加
             WHEN OTHER
                   MOVE 99999    TO   HWK-F013 SIH-F013 TER-F06
         END-EVALUATE
         MOVE   "4"               TO   TER-F07
         WRITE  TER-REC
         ADD    1                 TO   ERRT-CNT
     ELSE
              MOVE FUR-F03        TO   HWK-F013  SIH-F013
     END-IF.
*イオン取引先受信ワーク索引
     PERFORM  IONTWKF-READ-SEC.
     IF  IONTWKF-INV-FLG  =  "INV"
         MOVE SPACE               TO   TWK-REC
         INITIALIZE                    TWK-REC
         MOVE  "5"                TO   TWK-F01
         MOVE  PARA-HIDUKE        TO   TWK-F02
         MOVE  PARA-JIKAN         TO   TWK-F03
         MOVE  HWK-F013           TO   TWK-F04
         MOVE  1                  TO   TWK-F05
         WRITE  TWK-REC
     ELSE
         ADD   1                  TO   TWK-F05
         REWRITE  TWK-REC
     END-IF.
*流通ＢＭＳ発注メッセージレコードを削除する。
**** DELETE BMSSIHF.
*流通ＢＭＳ発注メッセージ初期化
**** MOVE     SPACE               TO   SIH-REC.
**** INITIALIZE                        SIH-REC.
*ワーク退避レコードをセット
**** MOVE     HWK-REC             TO   SIH-REC.
**** WRITE  SIH-REC.
     REWRITE  SIH-REC.
*付替件数をカウント
     ADD      1                   TO   HENK-CNT.
*流通ＢＭＳ支払メッセージ読込
     PERFORM  BMSSIHF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
*プログラム終了メッセージ表示
     MOVE      READ-CNT  TO      IN-CNT.
     MOVE      HENK-CNT  TO      OUT-CNT.
     MOVE      ERRT-CNT  TO      ERR-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-ERR   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     BMSSIHF  IONFURF  IONTWKF  IONTERWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　流通ＢＭＳ支払メッセージ読込
****************************************************************
 BMSSIHF-READ-SEC          SECTION.
*
     READ     BMSSIHF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO BMSSIHF-READ-EXIT
              NOT AT  END   ADD    1      TO  READ-CNT
     END-READ.
*件数表示
     IF       READ-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " READ-CNT " #" UPON CONS
     END-IF.
*指定バッチ_チェック
     IF     ( PARA-HIDUKE  =  SIH-F011 ) AND
            ( PARA-JIKAN   =  SIH-F012 ) AND
            ( PARA-TORICD  =  SIH-F013 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   BMSSIHF-READ-EXIT
     END-IF.
*
 BMSSIHF-READ-EXIT.
     EXIT.
****************************************************************
*　　イオン取引先振分マスタ索引
****************************************************************
 IONFURF-READ-SEC          SECTION.
*
*2013/01/31 NAV ST 支払法人ＣＤ→発注者ＣＤに変更
*****MOVE     SIH-F206     TO    FUR-F01.
     MOVE     SIH-F210     TO    FUR-F01.
*2013/01/31 NAV ED 支払法人ＣＤ→発注者ＣＤに変更
*店舗ＣＤを変換する。
*****MOVE     SIH-F405(1:4) TO   WK-TENPO.
*****MOVE     WK-TENPO-H   TO    FUR-F02.
*##2015/05/28 NAV ST 店舗５桁対応
*****MOVE     SIH-F405(1:4) TO   WK-TENPO.
*****MOVE     WK-TENPO-H   TO    FUR-F02.
     IF  SIH-F405(5:1)  =  SPACE
         MOVE SIH-F405(1:4) TO   WK-TENPO
         MOVE WK-TENPO-H    TO   FUR-F02
     ELSE
         MOVE SIH-F405(1:5) TO   WK-TENPO5
         MOVE WK-TENPO5-H   TO   FUR-F02
     END-IF.
*##2015/05/28 NAV ED
     READ     IONFURF
              INVALID       MOVE  "INV"   TO  IONFURF-INV-FLG
              NOT INVALID   MOVE  SPACE   TO  IONFURF-INV-FLG
     END-READ.
*
 IONFURF-READ-EXIT.
     EXIT.
****************************************************************
*　　イオン取引先受信ワーク
****************************************************************
 IONTWKF-READ-SEC           SECTION.
*
     MOVE     "5"           TO         TWK-F01.
     MOVE     HWK-F011      TO         TWK-F02.
     MOVE     HWK-F012      TO         TWK-F03.
     MOVE     HWK-F013      TO         TWK-F04.
     READ     IONTWKF
              INVALID       MOVE  "INV"   TO  IONTWKF-INV-FLG
              NOT INVALID   MOVE  SPACE   TO  IONTWKF-INV-FLG
     END-READ.
*
 IONTWKF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
