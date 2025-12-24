****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ダイユーエイトデータ変換　　　　　*
*    作成日／更新日　　　：　2010/07/05                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ダイユーエイトにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
*    作成日／更新日　　　：　2013/09/19                        *
*                            日敷追加　　　　　　　　　　　　　*
*    作成日／更新日　　　：　2018/01/29                        *
*                            リック資材／植物追加　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI7301B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/07/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*支払受信データファイル
     SELECT   DYSIHASF  ASSIGN    TO        DA-01-S-DYSIHASF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払明細ファイル
     SELECT   DYSIHF   ASSIGN    TO        DA-01-VI-DYSIHL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SIH-F11   SIH-F01
                                            SIH-F02   SIH-F03
                                            SIH-F04
                        FILE      STATUS    SIH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　1130 　  ＢＦ＝　3
******************************************************************
 FD  DYSIHASF
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(1127).
******************************************************************
*    支払データ
******************************************************************
 FD  DYSIHF
                        LABEL RECORD   IS   STANDARD.
     COPY     DYSIHF    OF        XFDLIB
              JOINING   SIH       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DYSIHAHD OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細情報格納領域
     COPY   DYSIHADT OF XFDLIB  JOINING   MEI  AS   PREFIX.
*トレーラ情報格納領域
     COPY   DYSIHATR OF XFDLIB  JOINING   TRE  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  IX1                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SIH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  JCA-CNT4                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-DENNO.
   03  WK-DENNO1             PIC  X(03)     VALUE  SPACE.
   03  WK-DENNO2             PIC  X(06)     VALUE  SPACE.
 01  WK-TEN                  PIC  X(13).
 01  WK-TEN-R     REDEFINES  WK-TEN.
   03  FILLER                PIC  9(08).
   03  WK-TENCD              PIC  9(05).
 01  WK-DENKIN               PIC  X(12).
 01  WK-GOKEI                PIC  X(12).
 01  WK-GOKEI-R   REDEFINES  WK-GOKEI.
*  03  FILERR                PIC  X(01).
   03  GOKEIKIN              PIC  S9(11).
*01  IX                      PIC  9(02).
 01  WK-DENKIN-AREA.
     03  WK-DENKIN-H         PIC  X(12).
     03  WK-DENKIN-F-R       REDEFINES    WK-DENKIN-H.
         05  WK-DENKIN-HEN   PIC  9(12).
     03  WK-DENKIN-FLG       PIC  9(01)   VALUE  ZERO.
     03  WK-DENKIN-OK        PIC S9(11)   VALUE  ZERO.
*
 01  WK-HACHU.
   03  WK-HACHUSYA           PIC  9(04)     VALUE  ZERO.
   03  FILLER                PIC  9(09)     VALUE  ZERO.
*
 01  WK-YMD.
   03  WK-YM                 PIC  9(06)     VALUE  ZERO.
   03  FILLER                PIC  9(02)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SIH-STATUS         PIC   X(02).
     03  EDI-STATUS         PIC   X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI7301B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI7301B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI7301B".
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
 01  WK-SOSAI-KINX.
     03  WK-SOSAI-KIN       PIC   9(11)  VALUE ZERO.
 01  WK-SOSAI               PIC  S9(11)  VALUE ZERO.
 01  WK-FUGO                PIC   X(01)  VALUE SPACE.
 01  WK-SOSAI-KIN-X.
     03  WK-SOSAI-X         OCCURS 12.
         05  WK-SOSAIX      PIC   X(01).
 01  WK-SOSAI-KIN-R.
     03  WK-SOSAI-R         OCCURS 12.
         05  WK-SOSAIR      PIC   9(01).
*
 01  WK1-SOSAI-KINX.
     03  WK1-SOSAI-KIN       PIC   9(12)  VALUE ZERO.
 01  WK1-SOSAI               PIC  S9(12)  VALUE ZERO.
 01  WK1-FUGO                PIC   X(01)  VALUE SPACE.
 01  WK1-SOSAI-KIN-X.
     03  WK1-SOSAI-X         OCCURS 12.
         05  WK1-SOSAIX      PIC   X(01).
 01  WK1-SOSAI-KIN-R.
     03  WK1-SOSAI-R         OCCURS 12.
         05  WK1-SOSAIR      PIC   9(01).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*LINKAGE                SECTION.
*01  PARA-KBN               PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE              DIVISION USING PARA-KBN.
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSIHASF.
     MOVE      "DYSIHASF"   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSIHF.
     MOVE      "DYSIHF"      TO   AB-FILE.
     MOVE      SIH-STATUS    TO   AB-STS.
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
              UNTIL     END-FLG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DYSIHASF.
     OPEN     OUTPUT    DYSIHF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        RD-CNT.
     MOVE     ZERO      TO        SIH-CNT.
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
*
     PERFORM  DYSIHASF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
           PERFORM  MEISAI-HEAD-SEC
     END-IF.
*
     IF    EDI-01  =   "DT"
           PERFORM  MEISAI-SIHARAI-SEC
     END-IF.
*
**** IF    EDI-01   =   "TR" AND
*****      MEI-M02  >   ZERO
*****      PERFORM  MEISAI-TRERA-SEC
**** END-IF.
*
     PERFORM DYSIHASF-READ-SEC.
*
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル入力　　　　　　　　　　　　　　　　　　*
****************************************************************
 DYSIHASF-READ-SEC      SECTION.
*
     MOVE "DYSIHASF-READ-SEC" TO       S-NAME.
*
     READ     DYSIHASF
              AT END
              MOVE     9      TO    END-FLG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 DYSIHASF-READ-EXIT.
     EXIT.
****************************************************************
*　　支払ヘッダレコード項目転送
****************************************************************
 MEISAI-HEAD-SEC    SECTION.
*
     MOVE    "HEAD-SEC"          TO   S-NAME.
*
***********ワークエリア初期化
     MOVE      SPACE       TO   SIH-REC  HED-REC.
     INITIALIZE                 SIH-REC  HED-REC.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   HED-REC.
*****支払ヘッダ作成
*    取引先ＣＤ
     MOVE   HED-H11        TO   WK-HACHU.
     EVALUATE  WK-HACHUSYA
*#2018/01/29 NAV ST
********WHEN   4950
        WHEN   4950   WHEN   4978
*#2018/01/29 NAV ED
               MOVE      HED-H03     TO   SIH-F00
*##2010/11/25 ﾀﾞｲﾕｰｴｲﾄMAXはﾀﾞｲﾕｰｴｲﾄに含める
        WHEN   4959
***************DISPLAY "MAXｼﾞｷﾞｮｳﾌﾞ ｱﾘ"
***************DISPLAY "TENPO1= " HED-H14
               MOVE      1225        TO   SIH-F00
        WHEN   4952
***************DISPLAY "TENPO2= " HED-H14
               MOVE      12251       TO   SIH-F00
*       2013/09/19 日敷　追加 ↓
        WHEN   4953
               MOVE      12252       TO   SIH-F00
*       2013/09/19 日敷　追加 ↑
     END-EVALUATE.
*    支払月
     MOVE      HED-H08         TO   WK-YMD.
     MOVE      WK-YM           TO   SIH-F01.
*    店舗ＣＤ
     MOVE      HED-H14     TO   SIH-F02.
*    支払期間開始
     MOVE      HED-H05     TO   SIH-F11.
*    支払期間終了
     MOVE      HED-H06     TO   SIH-F12.
*
 MEISAI-HEAD-EXIT.
     EXIT.
****************************************************************
*　　支払明細レコード項目転送
****************************************************************
 MEISAI-SIHARAI-SEC SECTION.
*
     MOVE    "SIHARAI-SEC"   TO   S-NAME.
***********ワークエリア初期化
     MOVE      SPACE       TO   MEI-REC.
     INITIALIZE                 MEI-REC.
     MOVE      SPACE       TO   END-FLG.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   MEI-REC.
*****支払明細項目
*    伝票日付
     MOVE      MEI-M07     TO   SIH-F03.
*    伝票区分
     MOVE      MEI-M08     TO   SIH-F04.
*    部門
     MOVE      MEI-M09     TO   SIH-F05.
*    伝票番号
     MOVE      MEI-M02     TO   SIH-F06.
*    漢字伝票区分名称
     MOVE      MEI-M10     TO   SIH-F09.
*    支払区分
     MOVE      1           TO   SIH-F10.
*相殺データ入力時、支払データ出力
     IF   MEI-M02   =  ZERO
          MOVE      2           TO   SIH-F10
*    相殺金額設定
          INITIALIZE                 WK-SOSAI-KIN-X
          INITIALIZE                 WK-SOSAI-KIN-R
          MOVE      MEI-M06     TO   WK-SOSAI-KIN-X
          MOVE      SPACE       TO   WK-FUGO
          PERFORM   VARYING  IX1  FROM  11  BY -1
                    UNTIL   IX1  <  1  OR  WK-FUGO = "1"
              IF    WK-SOSAIX(IX1)  =  "-"
                    MOVE   1    TO   WK-FUGO
              ELSE
                    MOVE  WK-SOSAIX(IX1)    TO  WK-SOSAIR(IX1)
              END-IF
          END-PERFORM
          MOVE   WK-SOSAI-KIN-R   TO  WK-SOSAI-KINX
          IF     WK-FUGO   =  "1"
                 COMPUTE  WK-SOSAI  =   WK-SOSAI-KIN * -1
          ELSE
                 MOVE     WK-SOSAI-KIN    TO   WK-SOSAI
          END-IF
          MOVE      WK-SOSAI              TO   SIH-F07
*    相殺コード
          EVALUATE  MEI-M08
              WHEN   10
                    MOVE     "0010"       TO   SIH-F08
              WHEN   11
                    MOVE     "0011"       TO   SIH-F08
              WHEN   12
                    MOVE     "0012"       TO   SIH-F08
              WHEN   13
                    MOVE     "0013"       TO   SIH-F08
              WHEN   14
                     MOVE     "0014"       TO   SIH-F08
              WHEN   15
                     MOVE     "0015"       TO   SIH-F08
              WHEN   16
                     MOVE     "0016"       TO   SIH-F08
              WHEN   17
                     MOVE     "0017"       TO   SIH-F08
              WHEN   18
                     MOVE     "0018"       TO   SIH-F08
              WHEN   19
                     MOVE     "0019"       TO   SIH-F08
              WHEN   20
                     MOVE     "0020"       TO   SIH-F08
              WHEN   21
                     MOVE     "0021"       TO   SIH-F08
              WHEN   22
                     MOVE     "0022"       TO   SIH-F08
              WHEN   23
                     MOVE     "0023"       TO   SIH-F08
              WHEN   24
                     MOVE     "0024"       TO   SIH-F08
              WHEN   25
                     MOVE     "0025"       TO   SIH-F08
              WHEN   26
                     MOVE     "0026"       TO   SIH-F08
              WHEN   27
                     MOVE     "0027"       TO   SIH-F08
              WHEN   28
                     MOVE     "0028"       TO   SIH-F08
              WHEN   29
                     MOVE     "0029"       TO   SIH-F08
              WHEN   30
                     MOVE     "0030"       TO   SIH-F08
              WHEN   31
                     MOVE     "0031"       TO   SIH-F08
              WHEN   32
                     MOVE     "0032"       TO   SIH-F08
              WHEN   33
                     MOVE     "0033"       TO   SIH-F08
              WHEN   34
                     MOVE     "0034"       TO   SIH-F08
              WHEN   35
                     MOVE     "0035"       TO   SIH-F08
          END-EVALUATE
     ELSE
*    相殺金額設定
          INITIALIZE                 WK-SOSAI-KIN-X
          INITIALIZE                 WK-SOSAI-KIN-R
**********DISPLAY "MEI-M04 = " MEI-M04 UPON CONS
          MOVE      MEI-M03     TO   WK-SOSAI-KIN-X
          MOVE      SPACE       TO   WK-FUGO
          PERFORM   VARYING  IX1  FROM  11  BY -1
                    UNTIL   IX1  <  1  OR  WK-FUGO = "1"
              IF    WK-SOSAIX(IX1)  =  "-"
                    MOVE   1    TO   WK-FUGO
              ELSE
                    MOVE  WK-SOSAIX(IX1)    TO  WK-SOSAIR(IX1)
              END-IF
          END-PERFORM
          MOVE   WK-SOSAI-KIN-R   TO  WK-SOSAI-KINX
          IF     WK-FUGO   =  "1"
                 COMPUTE  WK-SOSAI  =   WK-SOSAI-KIN * -1
          ELSE
                 MOVE     WK-SOSAI-KIN    TO   WK-SOSAI
          END-IF
          MOVE      WK-SOSAI              TO   SIH-F07
**********DISPLAY "WK1-SOSAI = " WK1-SOSAI UPON CONS
     END-IF.
*相殺レコード出力
     WRITE  SIH-REC.
     ADD    1              TO   SIH-CNT.
*
 MEISAI-SIHARAI-EXIT.
     EXIT.
****************************************************************
*　　支払トレーラレコード項目転送
****************************************************************
 MEISAI-TRERA-SEC   SECTION.
*
     MOVE    "TRERA-SEC"   TO   S-NAME.
***********ワークエリア初期化
     MOVE      SPACE       TO   TRE-REC.
     INITIALIZE                 TRE-REC.
     MOVE      SPACE       TO   END-FLG.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   TRE-REC.
*****支払ヘッダ作成
*    伝票合計金額
     IF        MEI-M08  NOT = 00  AND
               MEI-M08  NOT = 01  AND
               MEI-M08  NOT = 02  AND
               MEI-M08  NOT = 03  AND
               MEI-M08  NOT = 04  AND
               MEI-M08  NOT = 05  AND
               MEI-M08  NOT = 07  AND
               MEI-M08  NOT = 08  AND
               MEI-M08  NOT = 50  AND
               MEI-M08  NOT = 51  AND
               MEI-M08  NOT = 55  AND
               MEI-M08  NOT = 56  AND
               MEI-M08  NOT = 57
               MOVE      TRE-T09     TO   SIH-F07
     ELSE
               MOVE      TRE-T07     TO   SIH-F07
     END-IF.
*    相殺コード
     EVALUATE  MEI-M08
        WHEN   10
               MOVE     "0010"       TO   SIH-F08
        WHEN   11
               MOVE     "0011"       TO   SIH-F08
        WHEN   12
               MOVE     "0012"       TO   SIH-F08
        WHEN   13
               MOVE     "0013"       TO   SIH-F08
        WHEN   14
               MOVE     "0014"       TO   SIH-F08
        WHEN   15
               MOVE     "0015"       TO   SIH-F08
        WHEN   16
               MOVE     "0016"       TO   SIH-F08
        WHEN   17
               MOVE     "0017"       TO   SIH-F08
        WHEN   18
               MOVE     "0018"       TO   SIH-F08
        WHEN   19
               MOVE     "0019"       TO   SIH-F08
        WHEN   20
               MOVE     "0020"       TO   SIH-F08
        WHEN   21
               MOVE     "0021"       TO   SIH-F08
        WHEN   22
               MOVE     "0022"       TO   SIH-F08
        WHEN   23
               MOVE     "0023"       TO   SIH-F08
        WHEN   24
               MOVE     "0024"       TO   SIH-F08
        WHEN   25
               MOVE     "0025"       TO   SIH-F08
        WHEN   26
               MOVE     "0026"       TO   SIH-F08
        WHEN   27
               MOVE     "0027"       TO   SIH-F08
        WHEN   28
               MOVE     "0028"       TO   SIH-F08
        WHEN   29
               MOVE     "0029"       TO   SIH-F08
        WHEN   30
               MOVE     "0030"       TO   SIH-F08
        WHEN   31
               MOVE     "0031"       TO   SIH-F08
        WHEN   32
               MOVE     "0032"       TO   SIH-F08
        WHEN   33
               MOVE     "0033"       TO   SIH-F08
        WHEN   34
               MOVE     "0034"       TO   SIH-F08
        WHEN   35
               MOVE     "0035"       TO   SIH-F08
     END-EVALUATE.
*
     WRITE  SIH-REC.
     ADD    1              TO   SIH-CNT.
 MEISAI-TRERA-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     DYSIHF   DYSIHASF.
*
     DISPLAY NC"＃ＲＥＡＤ　ＣＮＴ＝"  RD-CNT    UPON CONS.
     DISPLAY NC"＃ＯＵＴ　　ＣＮＴ＝"  SIH-CNT   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
