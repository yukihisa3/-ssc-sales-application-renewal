# SSK0034V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0034V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　受領処理　　　　　　              *
*    モジュール名　　　　：　受領アンマッチＣＳＶデータ作成    *
*    作成日／作成者　　　：　14/03/17  YOSHIDA.M               *
*    処理概要　　　　　　：　ケーヨー様、出荷データ・受信データ*
*    　　　　　　　　　　　　マッチング結果より、指定された範囲*
*    　　　　　　　　　　　　の受領アンマッチＣＳＶデータを出力*
*    　　　　　　　　　　　　する。　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　14/04/11                          *
*    作成者／更新者　　　：　NAV高橋
*    処理概要　　　　　　：　アンマッチデータのキー変更　　　  *
*    　　　　　　　　　　　　出力レイアウト変更　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSK0034V.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<アンマッチデータ（出荷：受領） >>*************************
     SELECT   KEIUMCL1           ASSIGN    TO   DA-01-VI-KEIUMCL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
*********************************2014/04/11 NAV ST
*********************************RECORD  KEY    UMC-F01
*********************************               UMC-F02
*********************************               UMC-F03
                                 RECORD  KEY    UMC-F01
                                                UMC-F06
                                                UMC-F05
                                                UMC-F02
                                                UMC-F03
*********************************2014/04/11 NAV ED
                                 STATUS         UMC-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
*****<<受領CSVデータ >>*************************************
     SELECT   KEICSVUM           ASSIGN    TO   KEICSVUM
                                STATUS         CSV-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = アンマッチデータ（出荷：受領）　　　               *
*--------------------------------------------------------------*
 FD  KEIUMCL1           LABEL RECORD   IS   STANDARD.
     COPY     KEIUMCF   OF        XFDLIB
              JOINING   UMC       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 条件ファイル　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 受領データＣＳＶ　　　　　　　　                   *
*--------------------------------------------------------------*
 FD  KEICSVUM           BLOCK CONTAINS 1    RECORDS.
 01  CSV-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG              PIC  X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG       PIC  X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG      PIC  X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  UMC-STATUS           PIC  X(02).
 01  TEN-STATUS           PIC  X(02).
 01  JYO-STATUS           PIC  X(02).
 01  CSV-STATUS           PIC  X(02).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD           PIC  9(06)  VALUE  ZERO.
     03  SYS-DATEW        PIC  9(08)  VALUE  ZERO.
     03  SYS-DATE-R                   REDEFINES SYS-DATEW.
         05  SYS-YY       PIC  9(04).
         05  SYS-MM       PIC  9(02).
         05  SYS-DD       PIC  9(02).
***** システム時刻ワーク
 01  SYSTEM-TIME.
     03  SYS-HH           PIC  9(02).
     03  SYS-MN           PIC  9(02).
     03  SYS-SS           PIC  9(02).
***** カウンタ
 01  READ-CNT             PIC  9(07)  VALUE  ZERO.
 01  OUTPUT-CNT           PIC  9(07)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD1.
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(16)  VALUE
                          NC"＜ケーヨー受領アンマッチリスト＞".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"発行日：".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  HD01-YYYYMMDD    PIC  9(08).
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(05)  VALUE  NC"発行時刻：".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  HD01-HHMMSS      PIC  9(06).
 01  WK-HEAD2.
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"納品日：".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  HD02-NOHNBF      PIC  9(08).
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(01)  VALUE  NC"～".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  HD02-NOHNBT      PIC  9(08).
 01  WK-HEAD3.
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"店舗ＣＤ".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(03)  VALUE  NC"店舗名".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(03)  VALUE  NC"納品日".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(05)  VALUE  NC"サカタ伝区".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(06)  VALUE NC"ケーヨー伝区".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(03)  VALUE  NC"検収日".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(01)  VALUE  NC"行".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(05)  VALUE  NC"ＪＡＮＣＤ".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"出荷数量".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"出荷原価".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"受領数量".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(04)  VALUE  NC"受領原価".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(06)  VALUE  NC"メッセージ１".
     03  FILLER           PIC  X(01)  VALUE  X"29".
     03  FILLER           PIC  X(01)  VALUE  ",".
     03  FILLER           PIC  X(01)  VALUE  X"28".
     03  FILLER           PIC  N(06)  VALUE  NC"メッセージ２".
     03  FILLER           PIC  X(01)  VALUE  X"29".

*明細エリア
**************************2014/04/11 NAV ST ↓レイアウト変更
 01  WK-MEISAI1.
     03  CSV-A00          PIC  X(01).
     03  MD01-TENCD       PIC  9(05).
     03  CSV-A01          PIC  X(01).
     03  CSV-A01A         PIC  X(01).
     03  MD01-TENMEI      PIC  N(15).
     03  CSV-A01B         PIC  X(01).
     03  CSV-A02          PIC  X(01).
     03  MD01-NOHINBI     PIC  9(08).
     03  CSV-A03          PIC  X(01).
     03  MD01-SDNK        PIC  X(02).
     03  MD01-SDNKC       PIC  X(01).
     03  CSV-A03A         PIC  X(01).
     03  MD01-SDNKNM      PIC  N(05).
     03  CSV-A03B         PIC  X(01).
     03  CSV-A04          PIC  X(01).
     03  MD01-KDNK        PIC  X(02).
     03  MD01-KDNKC       PIC  X(01).
     03  CSV-A04A         PIC  X(01).
     03  MD01-KDNKNM      PIC  N(05).
     03  CSV-A04B         PIC  X(01).
     03  CSV-A05          PIC  X(01).
     03  MD01-KENSHUBI    PIC  9(08).
     03  CSV-A06          PIC  X(01).
     03  MD01-DENNO       PIC  9(09).
     03  CSV-A07          PIC  X(01).
     03  MD01-DENGYO      PIC  Z9.
     03  CSV-A08          PIC  X(01).
     03  MD01-JANCD       PIC  X(13).
     03  CSV-A09          PIC  X(01).
     03  MD01-SHUKASU     PIC  -----9.9.
     03  CSV-A10          PIC  X(01).
     03  MD01-SHKGEN      PIC  ------9.99.
     03  CSV-A11          PIC  X(01).
     03  MD01-JYURYOSU    PIC  -----9.9.
     03  CSV-A12          PIC  X(01).
     03  MD01-JYUGEN      PIC  ------9.99.
     03  CSV-A13          PIC  X(01).
     03  CSV-A13A         PIC  X(01).
     03  MD01-MSG1        PIC  N(10).
     03  CSV-A13B         PIC  X(01).
     03  CSV-A14          PIC  X(01).
     03  CSV-A14A         PIC  X(01).
     03  MD01-MSG2        PIC  N(10).
     03  CSV-A14B         PIC  X(01).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0034V".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0034V".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSK0034V".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
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
 LINKAGE                SECTION.
 01  PARA-SKBN              PIC   X(01).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-KKBN              PIC   X(01).
 01  PARA-TANFROM           PIC   X(02).
 01  PARA-TANTO             PIC   X(02).
 01  PARA-DENK1             PIC   X(02).
 01  PARA-DENK2             PIC   X(02).
 01  PARA-DENK3             PIC   X(02).
 01  PARA-DENK4             PIC   X(02).
 01  PARA-DENK5             PIC   X(02).
 01  PARA-TENFROM           PIC   9(05).
 01  PARA-TENTO             PIC   9(05).
 01  PARA-DENNFROM          PIC   9(09).
 01  PARA-DENNTO            PIC   9(09).
 01  PARA-SKBFROM           PIC   X(02).
 01  PARA-SKBTO             PIC   X(02).
 01  PARA-DENKFROM          PIC   X(02).
 01  PARA-DENKTO            PIC   X(02).
*
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-SKBN
                                           PARA-DKBN
                                           PARA-DFROM
                                           PARA-DTO
                                           PARA-KKBN
                                           PARA-TANFROM
                                           PARA-TANTO
                                           PARA-DENK1
                                           PARA-DENK2
                                           PARA-DENK3
                                           PARA-DENK4
                                           PARA-DENK5
                                           PARA-TENFROM
                                           PARA-TENTO
                                           PARA-DENNFROM
                                           PARA-DENNTO
                                           PARA-SKBFROM
                                           PARA-SKBTO
                                           PARA-DENKFROM
                                           PARA-DENKTO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           KEIUMCL1.
     MOVE     "KEIUMCL1"          TO        ERR-FL-ID.
     MOVE     UMC-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TENMS1.
     MOVE     "TENMS1  "          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN1.
     MOVE     "JYOKEN1 "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE KEICSVUM.
     MOVE     "KEICSVUM"          TO        ERR-FL-ID.
     MOVE     CSV-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO        PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSK0034V-START         SECTION.
*
     MOVE   "SSK0034V-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG   =  "END".
     PERFORM  END-SEC.
     STOP               RUN.
*
 SSK0034V-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KEIUMCL1.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     OUTPUT    KEICSVUM.
     DISPLAY  MSG-START UPON CONS.
*
*
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
     ACCEPT    SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   UMC-REC.
     INITIALIZE                      UMC-REC.
*
     PERFORM  KEIUMCL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し＃" UPON CONS
           MOVE  4001   TO PROGRAM-STATUS
           STOP  RUN
     END-IF.
     PERFORM  MIDASISET-SEC.
     MOVE     SPACE             TO   CSV-REC.
     MOVE     WK-HEAD1          TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE             TO   CSV-REC.
     MOVE     WK-HEAD2          TO   CSV-REC.
     WRITE    CSV-REC.
     MOVE     SPACE             TO   CSV-REC.
     MOVE     WK-HEAD3          TO   CSV-REC.
     WRITE    CSV-REC.
     ADD      3                 TO   OUTPUT-CNT.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　アンマッチデータ読み込み　　　
****************************************************************
 KEIUMCL1-RD-SEC            SECTION.
*
     MOVE    "KEIUMCL1-RD-SEC"    TO   S-NAME.
*
     READ     KEIUMCL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO                  TO   KEIUMCL1-RD-EXIT
     END-READ.
*
     ADD   1      TO  READ-CNT.
*
 KEIUMCL1-RD-EXIT.
     EXIT.
***************************************************************
*             店舗マスタ読込
***************************************************************
 TENMS1-READ-SEC        SECTION.
*
     MOVE    "TENMS1-READ-SEC"  TO        S-NAME.
*
     READ     TENMS1
              INVALID      MOVE  "INV"    TO   TENMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
***************************************************************
*             条件ファイル読込
***************************************************************
 JYOKEN1-READ-SEC       SECTION.
*
     MOVE    "JYOKEN1-READ-SEC" TO        S-NAME.
*
     READ     JYOKEN1
              INVALID      MOVE  "INV"    TO   JYOKEN1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   CSV-REC.
     MOVE     SPACE               TO   WK-MEISAI1.
     INITIALIZE                        WK-MEISAI1.
*項目転送
     MOVE   X"28"                 TO   CSV-A01A  CSV-A03A
                                       CSV-A04A  CSV-A13A
                                       CSV-A14A.
     MOVE   X"29"                 TO   CSV-A01B  CSV-A03B
                                       CSV-A04B  CSV-A13B
                                       CSV-A14B.
*
     MOVE     ","                 TO   CSV-A00   CSV-A01
                                       CSV-A02   CSV-A03
                                       CSV-A04   CSV-A05
                                       CSV-A06   CSV-A07
                                       CSV-A08   CSV-A09
                                       CSV-A10   CSV-A11
                                       CSV-A12   CSV-A13
                                       CSV-A14.
*
*  明細行編集
*  店舗CD
     MOVE     UMC-F01           TO    MD01-TENCD
*  店舗正式名
     MOVE     UMC-F01           TO    TEN-F011
     MOVE     173               TO    TEN-F52
     PERFORM  TENMS1-READ-SEC
     IF  TENMS1-INV-FLG = "INV"
       MOVE ALL NC"＊"      TO    MD01-TENMEI
     ELSE
       MOVE TEN-F02         TO    MD01-TENMEI
     END-IF.
*  伝票番号
     MOVE     UMC-F02              TO        MD01-DENNO.
*  行番
     MOVE     UMC-F03              TO        MD01-DENGYO.
*  サカタ伝区
     MOVE     UMC-F05              TO        MD01-SDNK.
     MOVE     ":"                  TO        MD01-SDNKC.
*  サカタ伝区名称
         MOVE     1                TO    JYO-F01
         MOVE     UMC-F05          TO    JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF   JYOKEN1-INV-FLG = "INV"
              MOVE  ALL NC"＊"     TO    MD01-SDNKNM
         ELSE
              MOVE  JYO-F03        TO    MD01-SDNKNM
         END-IF
*  ケーヨー伝区
     MOVE     UMC-F04              TO        MD01-KDNK.
     MOVE     ":"                  TO        MD01-KDNKC.
*  ケーヨー伝区名称
         MOVE     49               TO    JYO-F01
         MOVE     UMC-F04          TO    JYO-F02
         PERFORM  JYOKEN1-READ-SEC
         IF   JYOKEN1-INV-FLG = "INV"
**************2014/04/11 ↓変更 NAV ST
**************MOVE  ALL NC"＊"     TO    MD01-KDNKNM
              MOVE  NC"売上伝票"   TO    MD01-KDNKNM
**************2014/04/11 ↑変更 NAV ED
         ELSE
              MOVE  JYO-F03        TO    MD01-KDNKNM
         END-IF.
*  納品日
*****2014/04/11 NAV ST ↓納品予定日に変更
*****MOVE     UMC-F12              TO        MD01-NOHINBI.
     MOVE     UMC-F06              TO        MD01-NOHINBI.
*****2014/04/11 NAV ED ↑納品予定日に変更
*  検収日
     MOVE     UMC-F07              TO        MD01-KENSHUBI.
*  JANCD
     MOVE     UMC-F08              TO        MD01-JANCD.
*  出荷数量
     MOVE     UMC-F11              TO        MD01-SHUKASU.
*  出荷原価
     MOVE     UMC-F13              TO        MD01-SHKGEN.
*  受領数量
     MOVE     UMC-F09              TO        MD01-JYURYOSU.
*  受領原価
     MOVE     UMC-F10              TO        MD01-JYUGEN.
*  メッセージ１
     MOVE     UMC-F14              TO        MD01-MSG1.
*  メッセージ２
     MOVE     UMC-F15              TO        MD01-MSG2.
*レコードセット
     MOVE     WK-MEISAI1   TO   CSV-REC.
*
     WRITE    CSV-REC.
     ADD      1            TO   OUTPUT-CNT.
*    次レコード読込み
     PERFORM  KEIUMCL1-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"    TO   S-NAME.
*システム日付・時刻セット
     MOVE     SYS-DATE-R        TO   HD01-YYYYMMDD.
     MOVE     SYSTEM-TIME       TO   HD01-HHMMSS.
*納品日セット
     MOVE     PARA-DFROM        TO   HD02-NOHNBF.
     MOVE     PARA-DTO          TO   HD02-NOHNBT.
*
 MIDASISET-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    KEIUMCL1 TENMS1 JYOKEN1  KEICSVUM.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
