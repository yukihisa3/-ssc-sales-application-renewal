# SBT0340B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0340B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ抽出　　　　　　　  *
*                        ：　　（カインズオンライン）　　　　  *
*    作成日／更新日　　　：　2014/07/23                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを売上伝票データファイル*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2016/06/24                        *
*    変更概要　　　　　　：　NAV TAKAHASHI                     *
*      　　　　　　　　　　　数量訂正時、訂正区分＝無の場合、　*
*      　　　　　　　　　　　強制的に２１をセットする。　　　　*
*　　更新日／更新者　　　：　2022/03/14 NAV TAKAHASHI          *
*    変更概要　　　　　　：　２０分類変更に伴う改修            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0340B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/07/23.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE      STATUS    IS  DEN-ST.
*カインズ出荷連携データ
     SELECT   CNZSYKF   ASSIGN    TO        DA-01-S-CNZSYKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYU-ST.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    IS  MEI-ST.
*商品変換ＴＢＬ
     SELECT  SHOTBL1    ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01 SHO-F02
                        FILE      STATUS    IS  SHO-ST.
*流通ＢＭＳ発注ＭＳＧ
     SELECT  BMSHACL1   ASSIGN    TO        DA-01-VI-BMSHACL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HAC-F011  HAC-F012
                                            HAC-F013  HAC-F02
                                            HAC-F308  HAC-F346
                                            HAC-F302  HAC-F402
                        FILE      STATUS    IS  HAC-ST.
*欠品明細情報マスタ　
     SELECT  MSTKMEL1   ASSIGN    TO        DA-01-VI-MSTKMEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KME-F01   KME-F02
                        FILE      STATUS    IS  KME-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLA  OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    カインズ出荷連携データ
******************************************************************
 FD  CNZSYKF            BLOCK     CONTAINS   1   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     CNZSYKF   OF        XFDLIB
              JOINING   SYU       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1            LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    商品変換ＴＢＬ
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     SHOTBL1   OF       XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
*    流通ＢＭＳ発注ＭＳＧ
******************************************************************
 FD  BMSHACL1           LABEL RECORD   IS   STANDARD.
     COPY     BMSHACL1  OF       XFDLIB
              JOINING   HAC      PREFIX.
******************************************************************
*    欠品明細情報マスタ
******************************************************************
 FD  MSTKMEL1           LABEL RECORD   IS   STANDARD.
     COPY     MSTKMEL1  OF       XFDLIB
              JOINING   KME      PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  MEIMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  BMSHACL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  MSTKMEL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  999999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  DEN-ST        PIC  X(02).
     03  SYU-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
     03  SHO-ST        PIC  X(02).
     03  HAC-ST        PIC  X(02).
     03  KME-ST        PIC  X(02).
*バッチ_
 01  WK-BACHI-NO       PIC  X(20).
 01  WK-BACHI-NO-R         REDEFINES  WK-BACHI-NO.
     03  WK-BACHI-NO-1     PIC  9(08).
     03  WK-BACHI-NO-2     PIC  9(04).
     03  WK-BACHI-NO-3     PIC  9(08).
*部門コード退避
 01  WK-BUMON.
     03  WK-BUMON-1        PIC  9(04).
     03  WK-BUMON-2        PIC  9(08).
     03  WK-BUMON-3        PIC  9(04).
     03  WK-BUMON-4        PIC  X(04).
*店舗変換
 01  WK-TENPO-CD           PIC  9(06).
 01  WK-TENPO-CD-R         REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-CD-H     PIC  X(06).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

 01  MSG-AREA.
     03  MSG-WAKU           PIC  N(21)  VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-01             PIC  N(21)  VALUE
         NC"＊　以下の連携Ｎｏでデータを抽出します　＊".
     03  MSG-02.
         05  FILLER         PIC  X(04)  VALUE "＊".
         05  FILLER         PIC  X(12)  VALUE SPACE.
         05  MSG-02-RENNO   PIC  X(09).
         05  FILLER         PIC  X(17)  VALUE SPACE.
         05  FILLER         PIC  X(04)  VALUE "＊".

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  SYU-ERR           PIC  N(20)  VALUE
         NC"出荷連携データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SHO-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
     03  HAC-ERR           PIC  N(20)  VALUE
         NC"流通ＢＭＳ発注ＭＳＧエラー".
     03  KME-ERR           PIC  N(20)  VALUE
         NC"欠品明細情報マスタエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0340B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0340B".
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
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " KANRI  = ".
         05  OUT-CNT1       PIC   9(08).
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
* 入力パラメタ
*    部門CD
 01  PARA-IN-BUMONCD        PIC   X(04).
*    担当者CD
 01  PARA-IN-TANCD          PIC   X(02).
*    送信区分
 01  PARA-IN-SOUSIN-KB      PIC   X(01).
*    抽出区分（オンライン）
 01  PARA-IN-CYUSYUTU-ONL   PIC   X(01).
*    抽出区分（手書き）
 01  PARA-IN-CYUSYUTU-TEG   PIC   X(01).
*    抽出区分（横持）
 01  PARA-IN-CYUSYUTU-YOK   PIC   X(01).
*    抽出倉庫CD
 01  PARA-IN-SOUKO          PIC   X(02).
*    指定（オンライン）-バッチ_（受信日）
 01  PARA-IN-ONL-JUSIN-HI   PIC   9(08).
*    指定（オンライン）-バッチ_（受信時刻）
 01  PARA-IN-ONL-JUSIN-JI   PIC   9(04).
*    指定（オンライン）-バッチ_（取引先）
 01  PARA-IN-ONL-JUSIN-TOR  PIC   9(08).
*    指定（オンライン）-納品日（FROM）
 01  PARA-IN-ONL-NOUHIN-FR  PIC   9(08).
*    指定（オンライン）-納品日（TO）
 01  PARA-IN-ONL-NOUHIN-TO  PIC   9(08).
*    指定（オンライン）-店舗（FROM）
 01  PARA-IN-ONL-TENPO-FR   PIC   9(05).
*    指定（オンライン）-店舗（TO）
 01  PARA-IN-ONL-TENPO-TO   PIC   9(05).
*    指定（手書き）-取引先CD
 01  PARA-IN-TEG-TOR        PIC   9(08).
*    指定（手書き）-伝票NO（FROM）
 01  PARA-IN-TEG-DEN-FR     PIC   9(09).
*    指定（手書き）-伝票NO（TO）
 01  PARA-IN-TEG-DEN-TO     PIC   9(09).
*    指定（手書き）-納品日（FROM）
 01  PARA-IN-TEG-NOUHIN-FR  PIC   9(08).
*    指定（手書き）-納品日（TO）
 01  PARA-IN-TEG-NOUHIN-TO  PIC   9(08).
*    指定（横持ち）-横持日
 01  PARA-IN-YOK-HI         PIC   9(08).
* 出力パラメタ
*    抽出種類
 01  PARA-OUT-SYURUI        PIC   X(01).
*    抽出件数
 01  PARA-OUT-KENSUU        PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-BUMONCD
                            PARA-IN-TANCD
                            PARA-IN-SOUSIN-KB
                            PARA-IN-CYUSYUTU-ONL
                            PARA-IN-CYUSYUTU-TEG
                            PARA-IN-CYUSYUTU-YOK
                            PARA-IN-SOUKO
                            PARA-IN-ONL-JUSIN-HI
                            PARA-IN-ONL-JUSIN-JI
                            PARA-IN-ONL-JUSIN-TOR
                            PARA-IN-ONL-NOUHIN-FR
                            PARA-IN-ONL-NOUHIN-TO
                            PARA-IN-ONL-TENPO-FR
                            PARA-IN-ONL-TENPO-TO
                            PARA-IN-TEG-TOR
                            PARA-IN-TEG-DEN-FR
                            PARA-IN-TEG-DEN-TO
                            PARA-IN-TEG-NOUHIN-FR
                            PARA-IN-TEG-NOUHIN-TO
                            PARA-IN-YOK-HI
                            PARA-OUT-SYURUI
                            PARA-OUT-KENSUU.
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENLA.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZSYKF.
     DISPLAY     SYU-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYU-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MEIMS1.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     SHO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SHO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HAC-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BMSHACL1.
     DISPLAY     HAC-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HAC-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KME-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTKMEL1.
     DISPLAY     KME-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KME-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       SHTDENLA.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     BMSHACL1.
     OPEN     INPUT     MSTKMEL1.
     OPEN     EXTEND    CNZSYKF.
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
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    パラメタ　バッチ_変換
*    MOVE     PARA-IN-BACHI-NO   TO   WK-BACHI-NO.
*    DISPLAY "#ﾊﾞｯﾁNO. = " WK-BACHI-NO-1 "-"
*                          WK-BACHI-NO-2 "-"
*                          WK-BACHI-NO-3 " #"  UPON CONS.
*売上伝票ファイルスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-IN-ONL-JUSIN-HI  TO   DEN-F46.
     MOVE     PARA-IN-ONL-JUSIN-JI  TO   DEN-F47.
     MOVE     PARA-IN-ONL-JUSIN-TOR TO   DEN-F01.
     MOVE     PARA-IN-SOUKO         TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*売上伝票ファイル読込
     PERFORM  SHTDENLA-READ-SEC.
     IF       END-FLG = "END"
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
*レコード初期化
     MOVE     SPACE               TO   SYU-REC.
     INITIALIZE                        SYU-REC.
*
*【共通キー部】-------------------------------------------------
*　バッチ_－受信日
     MOVE     HAC-F011            TO   SYU-F011.
*  バッチ_－受信時刻
     MOVE     HAC-F012            TO   SYU-F012.
*  バッチ_－取引先コード
     MOVE     HAC-F013            TO   SYU-F013.
*  出荷場所
     MOVE     HAC-F02             TO   SYU-F02.
*  欠品区分   出荷数   ←→   訂正前数
     IF       DEN-F15  NOT =  DEN-F50
              PERFORM  MSTKMEL1-READ-SEC
              IF       MSTKMEL1-INV-FLG  =  "   "
                       MOVE  KME-F03        TO   SYU-F91
***********************2016/06/24 NAV ST
*※欠品が存在する場合、欠品区分＝０は有り得ないので２１セット※
                       IF  SYU-F91 = "00"
                           MOVE "21"        TO   SYU-F91
                       END-IF
***********************2016/06/24 NAV ED
              ELSE
*         マスタなしOR欠品区分が未入力の場合どう制御するか？
*         →訂正入力で未入力は許可していないので必ず値あり
***********************MOVE  "00"           TO   SYU-F91
*#2016/06/24 NAV ST 訂正区分セット
                       MOVE  "21"           TO   SYU-F91
*#2016/06/24 NAV ED 訂正区分セット
              END-IF
     ELSE
              MOVE     "00"                 TO   SYU-F91
     END-IF.
*  ONL手書区分
     MOVE     "0"                 TO   SYU-F92.
*  センター店舗直送区分
     IF       HAC-F330     = "01"
              MOVE     "1"        TO   SYU-F93
     ELSE
              MOVE     "0"        TO   SYU-F93
     END-IF.
*  商品名称マスタJANCD
     MOVE     MEI-F06             TO   SYU-F94.
*  タネ／資材区分
     MOVE     TANE-SIZAI          TO   SYU-F95.
*  ラベル張替区分
     IF       SHO-F10     =       " "      OR   "2"
              MOVE     "0"                 TO   SYU-F96
     ELSE
              MOVE     SHO-F10             TO   SYU-F96
     END-IF.
*  張替JＡＮＣＤ
     IF       SHO-F10     =       "1"
              MOVE     SHO-F02             TO   SYU-F97
     ELSE
              MOVE     SPACE               TO   SYU-F97
     END-IF.
*  受信者部門コード
     MOVE     HAC-F98             TO   SYU-F98.
*  受信者担当者コード
     MOVE     HAC-F99             TO   SYU-F99.
*
*【メッセージヘッダ部】レコード区分"A"--------------------------
*  レコード区分
     MOVE     HAC-F101            TO   SYU-F101.
*  ヘッダバージョン
     MOVE     HAC-F102            TO   SYU-F102.
*  送信者ＩＤ
     MOVE     HAC-F103            TO   SYU-F103.
*  送信者ＩＤ発行元
     MOVE     HAC-F104            TO   SYU-F104.
*  受信者ＩＤ
     MOVE     HAC-F105            TO   SYU-F105.
*  受信者ＩＤ発行元
     MOVE     HAC-F106            TO   SYU-F106.
*  流通ＢＭＳ名称
     MOVE     HAC-F107            TO   SYU-F107.
*  バージョン
     MOVE     HAC-F108            TO   SYU-F108.
*  インスタンスＩＤ
     MOVE     HAC-F109            TO   SYU-F109.
*  メッセージ種
     MOVE     HAC-F110            TO   SYU-F110.
*  複合メッセージフラグ
     MOVE     HAC-F111            TO   SYU-F111.
*  作成日時
     MOVE     HAC-F112            TO   SYU-F112.
*  テスト区分タイプ
     MOVE     HAC-F113            TO   SYU-F113.
*  テスト区分インスタンスＩＤ
     MOVE     HAC-F114            TO   SYU-F114.
*  テスト区分ＩＤ
     MOVE     HAC-F115            TO   SYU-F115.
*  最終送信先タイプ
     MOVE     HAC-F116            TO   SYU-F116.
*  最終送信先インスタンスＩＤ
     MOVE     HAC-F117            TO   SYU-F117.
*  最終送信先ＩＤ
     MOVE     HAC-F118            TO   SYU-F118.
*  メッセージ識別ＩＤ
     MOVE     HAC-F119            TO   SYU-F119.
*  送信者ステーションアドレス
     MOVE     HAC-F120            TO   SYU-F120.
*  最終受信者ステーションアドレス
     MOVE     HAC-F121            TO   SYU-F121.
*  直接受信者ステーションアドレス
     MOVE     HAC-F122            TO   SYU-F122.
*  取引件数
     MOVE     HAC-F123            TO   SYU-F123.
     IF       HAC-F123            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F123
     END-IF.
*
*【発注リスト部】レコード区分"B"--------------------------------
*  レコード区分
     MOVE     HAC-F201            TO   SYU-F201.
*  ＸＭＬ内容バージョンＩＤ
     MOVE     HAC-F202            TO   SYU-F202.
*  ＸＭＬ構造バージョンＩＤ
     MOVE     HAC-F203            TO   SYU-F203.
*  拡張情報ネームスペース
     MOVE     HAC-F204            TO   SYU-F204.
*  拡張情報バージョン番号
     MOVE     HAC-F205            TO   SYU-F205.
*  支払法人コード
     MOVE     HAC-F206            TO   SYU-F206.
*  支払法人ＧＬＮ
     MOVE     HAC-F207            TO   SYU-F207.
*  支払法人名称
     MOVE     X"28"               TO   SYU-F2081.
     MOVE     HAC-F208            TO   SYU-F208.
     MOVE     X"29"               TO   SYU-F2082.
*  支払法人名称カナ
     MOVE     HAC-F209            TO   SYU-F209.
*  発注者コード
     MOVE     HAC-F210            TO   SYU-F210.
*  発注者ＧＬＮ
     MOVE     HAC-F211            TO   SYU-F211.
*  発注者名称
     MOVE     X"28"               TO   SYU-F2121.
     MOVE     HAC-F212            TO   SYU-F212.
     MOVE     X"29"               TO   SYU-F2122.
*  発注者名称カナ
     MOVE     HAC-F213            TO   SYU-F213.
*
*【取引部】レコード区分"C"--------------------------------------
*  レコード区分
     MOVE     HAC-F301            TO   SYU-F301.
*  取引番号（発注・返品）
     MOVE     HAC-F302            TO   SYU-F302.
*  取引付属番号
     MOVE     HAC-F303            TO   SYU-F303.
*  直接納品先コード
     MOVE     HAC-F304            TO   SYU-F304.
*  直接納品先ＧＬＮ
     MOVE     HAC-F305            TO   SYU-F305.
*  直接納品先名称
     MOVE     X"28"               TO   SYU-F3061.
     MOVE     HAC-F306            TO   SYU-F306.
     MOVE     X"29"               TO   SYU-F3062.
*  直接納品先名称カナ
     MOVE     HAC-F307            TO   SYU-F307.
*  最終納品先コード
     MOVE     HAC-F308            TO   SYU-F308.
*  最終納品先ＧＬＮ
     MOVE     HAC-F309            TO   SYU-F309.
*  最終納品先名称
     MOVE     X"28"               TO   SYU-F3101.
     MOVE     HAC-F310            TO   SYU-F310.
     MOVE     X"29"               TO   SYU-F3102.
*  最終納品先名称カナ
     MOVE     HAC-F311            TO   SYU-F311.
*  計上部署コード
     MOVE     HAC-F312            TO   SYU-F312.
*  計上部署ＧＬＮ
     MOVE     HAC-F313            TO   SYU-F313.
*  計上部署名称
     MOVE     X"28"               TO   SYU-F3141.
     MOVE     HAC-F314            TO   SYU-F314.
     MOVE     X"29"               TO   SYU-F3142.
*  計上部署名称カナ
     MOVE     HAC-F315            TO   SYU-F315.
*  陳列場所コード
     MOVE     HAC-F316            TO   SYU-F316.
*  陳列場所名称
     MOVE     X"28"               TO   SYU-F3171.
     MOVE     HAC-F317            TO   SYU-F317.
     MOVE     X"29"               TO   SYU-F3172.
*  陳列場所名称カナ
     MOVE     HAC-F318            TO   SYU-F318.
*  請求取引先コード
     MOVE     HAC-F319            TO   SYU-F319.
*  請求取引先ＧＬＮ
     MOVE     HAC-F320            TO   SYU-F320.
*  請求取引先名
     MOVE     X"28"               TO   SYU-F3211.
     MOVE     HAC-F321            TO   SYU-F321.
     MOVE     X"29"               TO   SYU-F3212.
*  請求取引先名カナ
     MOVE     HAC-F322            TO   SYU-F322.
*  取引先コード
     MOVE     HAC-F323            TO   SYU-F323.
*  取引先ＧＬＮ
     MOVE     HAC-F324            TO   SYU-F324.
*  取引先名称
     MOVE     X"28"               TO   SYU-F3251.
     MOVE     HAC-F325            TO   SYU-F325.
     MOVE     X"29"               TO   SYU-F3252.
*  取引先名称カナ
     MOVE     HAC-F326            TO   SYU-F326.
*  枝番
     MOVE     HAC-F327            TO   SYU-F327.
*  出荷先コード
     MOVE     HAC-F328            TO   SYU-F328.
*  出荷場所ＧＬＮ
     MOVE     HAC-F329            TO   SYU-F329.
*  納品経路
     MOVE     HAC-F330            TO   SYU-F330.
*  便NO
     MOVE     HAC-F331            TO   SYU-F331.
*  通過在庫区分
     MOVE     HAC-F332            TO   SYU-F332.
*  納品区分
     MOVE     HAC-F333            TO   SYU-F333.
*  指定納品時刻
     MOVE     HAC-F334            TO   SYU-F334.
*  輸送手段
     MOVE     HAC-F335            TO   SYU-F335.
*  バーコード情報
     MOVE     HAC-F336            TO   SYU-F336.
*  カテゴリ名称１（印字用）
     MOVE     X"28"               TO   SYU-F3371.
     MOVE     HAC-F337            TO   SYU-F337.
     MOVE     X"29"               TO   SYU-F3372.
*  カテゴリ名称２（印字用）
     MOVE     X"28"               TO   SYU-F3381.
     MOVE     HAC-F338            TO   SYU-F338.
     MOVE     X"29"               TO   SYU-F3382.
*  最終納品先名称（印字用）
     MOVE     X"28"               TO   SYU-F3391.
     MOVE     HAC-F339            TO   SYU-F339.
     MOVE     X"29"               TO   SYU-F3392.
*  ラベル自由使用欄（印字用）
     MOVE     X"28"               TO   SYU-F3401.
     MOVE     HAC-F340            TO   SYU-F340.
     MOVE     X"29"               TO   SYU-F3402.
*  ラベル自由使用欄半角カナ（印字用）
     MOVE     HAC-F341            TO   SYU-F341.
*  商品分類（大）
     MOVE     HAC-F342            TO   SYU-F342.
*  商品分類（中）
     MOVE     HAC-F343            TO   SYU-F343.
*  発注日
     MOVE     HAC-F344            TO   SYU-F344.
     IF       HAC-F344            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F344
     END-IF.
*  直接納品先納品日
     MOVE     HAC-F345            TO   SYU-F345.
     IF       HAC-F345            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F345
     END-IF.
*  最終納品先納品日
     MOVE     HAC-F346            TO   SYU-F346.
     IF       HAC-F346            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F346
     END-IF.
*  計上日
     MOVE     HAC-F347            TO   SYU-F347.
     IF       HAC-F347            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F347
     END-IF.
*  販促開始日
     MOVE     HAC-F348            TO   SYU-F348.
     IF       HAC-F348            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F348
     END-IF.
*  販促終了日
     MOVE     HAC-F349            TO   SYU-F349.
     IF       HAC-F349            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F349
     END-IF.
*  取引（発注・返品）データ有効日
     MOVE     HAC-F350            TO   SYU-F350.
     IF       HAC-F350            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F350
     END-IF.
*  商品区分
     MOVE     HAC-F351            TO   SYU-F351.
*  発注区分
     MOVE     HAC-F352            TO   SYU-F352.
*  出荷データ有無区分
     MOVE     HAC-F353            TO   SYU-F353.
*  ＰＢ区分
     MOVE     HAC-F354            TO   SYU-F354.
*  配送温度区分
     MOVE     HAC-F355            TO   SYU-F355.
*  酒区分
     MOVE     HAC-F356            TO   SYU-F356.
*  処理種別
     MOVE     HAC-F357            TO   SYU-F357.
*  伝票レス区分
     MOVE     HAC-F358            TO   SYU-F358.
*  取引番号区分
     MOVE     HAC-F359            TO   SYU-F359.
*  パック区分
     MOVE     HAC-F360            TO   SYU-F360.
*  不定貫区分
     MOVE     HAC-F361            TO   SYU-F361.
*  税区分
     MOVE     HAC-F362            TO   SYU-F362.
*  税率
     MOVE     HAC-F363            TO   SYU-F363.
     IF       HAC-F363            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F363
     END-IF.
*  自由使用欄
     MOVE     X"28"               TO   SYU-F3641.
     MOVE     HAC-F364            TO   SYU-F364.
     MOVE     X"29"               TO   SYU-F3642.
*  自由使用欄半角カナ
     MOVE     HAC-F365            TO   SYU-F365.
*  原価金額合計
     MOVE     HAC-F366            TO   SYU-F366.
     IF       HAC-F366            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F366
     END-IF.
*  売価金額合計
     MOVE     HAC-F367            TO   SYU-F367.
     IF       HAC-F367            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F367
     END-IF.
*  税額金額合計
     MOVE     HAC-F368            TO   SYU-F368.
     IF       HAC-F368            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F368
     END-IF.
*  数量合計
     MOVE     HAC-F369            TO   SYU-F369.
     IF       HAC-F369            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F369
     END-IF.
*  発注単位数量合計
     MOVE     HAC-F370            TO   SYU-F370.
     IF       HAC-F370            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F370
     END-IF.
*  重量合計
     MOVE     HAC-F371            TO   SYU-F371.
     IF       HAC-F371            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F371
     END-IF.
*
*【取引明細部】レコード区分"D"----------------------------------
*  レコード区分
     MOVE     HAC-F401            TO   SYU-F401.
*  取引明細番号（発注・返品）
     MOVE     HAC-F402            TO   SYU-F402.
*  取引付属明細番号
     MOVE     HAC-F403            TO   SYU-F403.
*  元取引番号
     MOVE     HAC-F404            TO   SYU-F404.
*  元取引明細番号
     MOVE     HAC-F405            TO   SYU-F405.
*  商品分類（小）
     MOVE     HAC-F406            TO   SYU-F406.
*  商品分類（細）
     MOVE     HAC-F407            TO   SYU-F407.
*  配達予定日
     MOVE     HAC-F408            TO   SYU-F408.
     IF       HAC-F408            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F408
     END-IF.
*  納品期限
     MOVE     HAC-F409            TO   SYU-F409.
     IF       HAC-F409            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F409
     END-IF.
*  センター納品詳細指示
     MOVE     HAC-F410            TO   SYU-F410.
*  メーカーコード
     MOVE     HAC-F411            TO   SYU-F411.
*  商品コード（ＧＴＩＮ）
     MOVE     HAC-F412            TO   SYU-F412.
*  商品コード（発注用）
     MOVE     HAC-F413            TO   SYU-F413.
*  商品コード区分
     MOVE     HAC-F414            TO   SYU-F414.
*  商品コード（取引先）
     MOVE     HAC-F415            TO   SYU-F415.
*  商品名
     MOVE     X"28"               TO   SYU-F4161.
     MOVE     HAC-F416            TO   SYU-F416.
     MOVE     X"29"               TO   SYU-F4162.
*  商品名カナ
     MOVE     HAC-F417            TO   SYU-F417.
*  規格
     MOVE     X"28"               TO   SYU-F4181.
     MOVE     HAC-F418            TO   SYU-F418.
     MOVE     X"29"               TO   SYU-F4182.
*  規格カナ
     MOVE     HAC-F419            TO   SYU-F419.
*  カラーコード
     MOVE     HAC-F420            TO   SYU-F420.
*  カラー名称
     MOVE     X"28"               TO   SYU-F4211.
     MOVE     HAC-F421            TO   SYU-F421.
     MOVE     X"29"               TO   SYU-F4212.
*  カラー名称カナ
     MOVE     HAC-F422            TO   SYU-F422.
*  サイズコード
     MOVE     HAC-F423            TO   SYU-F423.
*  サイズ名称
     MOVE     X"28"               TO   SYU-F4241.
     MOVE     HAC-F424            TO   SYU-F424.
     MOVE     X"29"               TO   SYU-F4242.
*  サイズ名称カナ
     MOVE     HAC-F425            TO   SYU-F425.
*  入数
     MOVE     HAC-F426            TO   SYU-F426.
     IF       HAC-F426            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F426
     END-IF.
*  都道府県コード
     MOVE     HAC-F427            TO   SYU-F427.
*  国コード
     MOVE     HAC-F428            TO   SYU-F428.
*  産地名
     MOVE     X"28"               TO   SYU-F4291.
     MOVE     HAC-F429            TO   SYU-F429.
     MOVE     X"29"               TO   SYU-F4292.
*  水域コード
     MOVE     HAC-F430            TO   SYU-F430.
*  水域名
     MOVE     X"28"               TO   SYU-F4311.
     MOVE     HAC-F431            TO   SYU-F431.
     MOVE     X"29"               TO   SYU-F4312.
*  原産エリア
     MOVE     X"28"               TO   SYU-F4321.
     MOVE     HAC-F432            TO   SYU-F432.
     MOVE     X"29"               TO   SYU-F4322.
*  等級
     MOVE     X"28"               TO   SYU-F4331.
     MOVE     HAC-F433            TO   SYU-F433.
     MOVE     X"29"               TO   SYU-F4332.
*  階級
     MOVE     X"28"               TO   SYU-F4341.
     MOVE     HAC-F434            TO   SYU-F434.
     MOVE     X"29"               TO   SYU-F4342.
*  銘柄
     MOVE     X"28"               TO   SYU-F4351.
     MOVE     HAC-F435            TO   SYU-F435.
     MOVE     X"29"               TO   SYU-F4352.
*  商品ＰＲ
     MOVE     X"28"               TO   SYU-F4361.
     MOVE     HAC-F436            TO   SYU-F436.
     MOVE     X"29"               TO   SYU-F4362.
*  バイオ区分
     MOVE     HAC-F437            TO   SYU-F437.
*  品種コード
     MOVE     HAC-F438            TO   SYU-F438.
*  養殖区分
     MOVE     HAC-F439            TO   SYU-F439.
*  解凍区分
     MOVE     HAC-F440            TO   SYU-F440.
*  商品状態区分
     MOVE     HAC-F441            TO   SYU-F441.
*  形状・部位
     MOVE     X"28"               TO   SYU-F4421.
     MOVE     HAC-F442            TO   SYU-F442.
     MOVE     X"29"               TO   SYU-F4422.
*  用途
     MOVE     X"28"               TO   SYU-F4431.
     MOVE     HAC-F443            TO   SYU-F443.
     MOVE     X"29"               TO   SYU-F4432.
*  決定管理義務商材区分
     MOVE     HAC-F444            TO   SYU-F444.
*  原価金額
     MOVE     HAC-F445            TO   SYU-F445.
     IF       HAC-F445            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F445
     END-IF.
*  原単価
     MOVE     HAC-F446            TO   SYU-F446.
     IF       HAC-F446            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F446
     END-IF.
*  売価金額
     MOVE     HAC-F447            TO   SYU-F447.
     IF       HAC-F447            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F447
     END-IF.
*  売単価
     MOVE     HAC-F448            TO   SYU-F448.
     IF       HAC-F448            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F448
     END-IF.
*  税額
     MOVE     HAC-F449            TO   SYU-F449.
     IF       HAC-F449            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F449
     END-IF.
*  発注単位
     MOVE     HAC-F450            TO   SYU-F450.
     IF       HAC-F450            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F450
     END-IF.
*  発注単位コード
     MOVE     HAC-F451            TO   SYU-F451.
*  発注荷姿コード
     MOVE     HAC-F452            TO   SYU-F452.
*  発注数量（バラ）
     MOVE     HAC-F453            TO   SYU-F453.
     IF       HAC-F453            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F453
     END-IF.
*  発注数量（発注単位数）
     MOVE     HAC-F454            TO   SYU-F454.
     IF       HAC-F454            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F454
     END-IF.
*  取引単位重量
     MOVE     HAC-F455            TO   SYU-F455.
     IF       HAC-F455            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F455
     END-IF.
*  単価登録単位
     MOVE     HAC-F456            TO   SYU-F456.
*  商品重量
     MOVE     HAC-F457            TO   SYU-F457.
     IF       HAC-F457            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F457
     END-IF.
*  発注重量
     MOVE     HAC-F458            TO   SYU-F458.
     IF       HAC-F458            NOT  NUMERIC
              MOVE     ZERO       TO   SYU-F458
     END-IF.
*
*注）以降の項目は、発注ＭＳＧには値が入っていないため、
*　　転送しても初期値がセットされるだけである。
*　　必要に応じて個別にセットする項目である。
*【出荷関連情報－取引】レコード区分"C"--------------------------
*  出荷者管理番号
     MOVE     HAC-F501            TO   SYU-F501.
*  入荷管理用メーカーコード
     MOVE     HAC-F502            TO   SYU-F502.
*  センター納品書番号
     MOVE     HAC-F503            TO   SYU-F503.
*  ＥＯＳ区分
     MOVE     HAC-F504            TO   SYU-F504.
*
*【出荷関連情報－取引明細】レコード区分"D"----------------------
*  出荷者管理明細番号
     MOVE     HAC-F601            TO   SYU-F601.
*  仮伝フラグ
     MOVE     HAC-F602            TO   SYU-F602.
*  商品コード（出荷元）
     MOVE     HAC-F603            TO   SYU-F603.
*  出荷数量（バラ）
     MOVE     DEN-F15             TO   SYU-F604.
*  出荷数量（発注単位数）
*****MOVE     HAC-F605            TO   SYU-F605.
*****IF       HAC-F450  =   "1"
     DIVIDE   DEN-F15  BY HAC-F450 GIVING  WK-DEN-F15
                                           REMAINDER   AMARI.
     IF       AMARI    NOT = 0
              MOVE     HAC-F450   TO  WK-MEI-F07
              MOVE     DEN-F15    TO  WK-SUURYOU
              DISPLAY  NC"数量が入数で割り切れません！"
                        " !"  NC"商Ｃ＝" DEN-F141
                         "!"  NC"入数＝" WK-MEI-F07
                         "!"  NC"数量＝" WK-SUURYOU
                         "!"  NC"取Ｃ＝" DEN-F01
                         "!"  NC"店舗＝" DEN-F07
                         "!"  NC"納日＝" DEN-F112
                         "!"  NC"伝票＝" DEN-F02
                         "!"  NC"行＝" DEN-F03
              GO       TO             MAIN-99
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F605.
*  欠品数量（バラ）
     COMPUTE  SYU-F606   =    DEN-F50  -  DEN-F15.
     IF       DEN-F50    <    DEN-F15
              MOVE  0    TO   HAC-F606
     END-IF.
*  欠品数量（発注単位数）
*    MOVE     HAC-F607            TO   SYU-F607.
     MOVE     ZERO                TO   SYU-F607.
*  欠品区分   共通部の欠品区分と同一
     MOVE     SYU-F91             TO   SYU-F608.
*  出荷重量
*    MOVE     HAC-F609            TO   SYU-F609.
     MOVE     ZERO                TO   SYU-F609.
*
*【出荷関連情報－出荷荷姿情報】レコード区分"E"------------------
*  ＩＴＦコード（集合包装ＧＴＩＮ）
     MOVE     HAC-F701            TO   SYU-F701.
*  出荷荷姿コード
     MOVE     HAC-F702            TO   SYU-F702.
*  出荷数量（出荷荷姿数）
*    MOVE     HAC-F703            TO   SYU-F703.
     MOVE     ZERO                TO   SYU-F703.
*  賞味期限日
*    MOVE     HAC-F704            TO   SYU-F704.
     MOVE     ZERO                TO   SYU-F704.
*  製造日
*    MOVE     HAC-F705            TO   SYU-F705.
     MOVE     ZERO                TO   SYU-F705.
*  製造番号
*    MOVE     HAC-F706            TO   SYU-F706.
     MOVE     ZERO                TO   SYU-F706.
*
*【出荷関連情報－処理制御】-------------------------------------
*  出荷情報抽出済ＦＬＧ
*             引き渡す項目ではない
*  抽出日
     MOVE     SYS-DATEW           TO   SYU-F802.
*  抽出時刻
     MOVE     SYS-HHMMSS          TO   SYU-F803.
*2014/08/18 NAV ST ３６００バイトで送信するため
     MOVE     "1"                 TO   SYU-FIL7(100:1).
*2014/08/18 NAV ED ３６００バイトNO
*----------------------------------------------------------------
*
*品名コード
*    IF       SHO-F10     =       "2"
*             MOVE       DEN-F25  TO   SYU-F14
*    ELSE
*             IF         MEI-F06  =    SPACE
*                    DISPLAY  NC"ＪＡＮＣＤ取得エラー！！"
*
*                      " !"   NC"取＝" DEN-F01
*                       "!"   NC"伝＝" DEN-F02
*                       "!"   NC"行＝" DEN-F03
*                       "!"   NC"店＝" DEN-F07
*                       "!"   NC"納＝" DEN-F112
*                       "!"   NC"サ商＝" DEN-F141
*                       "!"   NC"相商＝" DEN-F25
*                        GO       TO   MAIN-99
*             ELSE
*                        MOVE     MEI-F06   TO   SYU-F14
*             END-IF
*    END-IF.
*出荷指示数
*    IF       MEI-F89   =   "1"
*             DIVIDE  DEN-F50  BY MEI-F07  GIVING  WK-DEN-F15
*                                          REMAINDER   AMARI
*             IF     AMARI    NOT = 0
*                    MOVE    MEI-F07    TO  WK-MEI-F07
*                    MOVE    DEN-F50    TO  WK-SUURYOU
*                    DISPLAY  NC"数量が入数で割り切れません！"
*
*                       " !"  NC"商Ｃ＝" MEI-F01
*                        "!"  NC"入数＝" WK-MEI-F07
*                        "!"  NC"数量＝" WK-SUURYOU
*                        "!"  NC"取Ｃ＝" DEN-F01
*                        "!"  NC"店舗＝" DEN-F07
*                        "!"  NC"納日＝" DEN-F112
*                        "!"  NC"伝票＝" DEN-F02
*                        "!"  NC"行＝" DEN-F03
*                    GO       TO             MAIN-99
*             END-IF
*    ELSE
*             MOVE    DEN-F50             TO  WK-DEN-F15
*    END-IF.
*    MOVE     WK-DEN-F15          TO   SYU-F15.
*実出荷数
*    IF       MEI-F89   =   "1"
*             DIVIDE  DEN-F15  BY MEI-F07  GIVING  WK-DEN-F50
*                                          REMAINDER   AMARI
*             IF      AMARI    NOT = 0
*                    MOVE    MEI-F07    TO  WK-MEI-F07
*                    MOVE    DEN-F15    TO  WK-SUURYOU
*                    DISPLAY  NC"数量が入数で割り切れません！"
*
*                       " !" NC"商Ｃ＝" MEI-F01
*                        "!" NC"入数＝" WK-MEI-F07
*                        "!" NC"数量＝" WK-SUURYOU
*                        "!" NC"取Ｃ＝" DEN-F01
*                        "!" NC"店舗＝" DEN-F07
*                        "!" NC"納日＝" DEN-F112
*                        "!" NC"伝票＝" DEN-F02
*                        "!" NC"行＝" DEN-F03
*                     GO       TO             MAIN-99
*             END-IF
*    ELSE
*             MOVE    DEN-F15             TO  WK-DEN-F50
*    END-IF.
*    MOVE     WK-DEN-F50          TO   SYU-F16.
*
*出荷連携データ出力
     WRITE    SYU-REC.
*売上伝票ファイルへ連携済FLG更新
     MOVE     "1"                 TO   DEN-F68.
     MOVE     SYS-DATEW           TO   DEN-F69.
     REWRITE  DEN-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*売上伝票ファイル読込
 MAIN-99.
     PERFORM  SHTDENLA-READ-SEC.
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
     IF  WRT-CNT1 > ZERO
         MOVE "K"                       TO PARA-OUT-SYURUI
         MOVE WRT-CNT1                  TO PARA-OUT-KENSUU
     END-IF.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT1  TO      OUT-CNT.
*    MOVE      WRT-CNT2  TO      OUT-CNT1.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
*    DISPLAY   MSG-OUT1  UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     SHTDENLA  CNZSYKF  MEIMS1  SHOTBL1
               BMSHACL1  MSTKMEL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENLA-READ-SEC          SECTION.
*
     READ     SHTDENLA
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SHTDENLA-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*指定バッチ_チェック
 SHTDENLA-READ-1.
     IF     ( PARA-IN-ONL-JUSIN-HI  =  DEN-F46 ) AND
            ( PARA-IN-ONL-JUSIN-JI  =  DEN-F47 ) AND
            ( PARA-IN-ONL-JUSIN-TOR =  DEN-F01 )
              CONTINUE
     ELSE
*TEST↓
*             DISPLAY "BACH-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLA-READ-EXIT
     END-IF.
*抽出倉庫チェック（オンライン：振分倉庫）
 SHTDENLA-READ-2.
     IF       PARA-IN-SOUKO  =   DEN-F48
              CONTINUE
     ELSE
*TEST↓
*             DISPLAY "SOKO-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLA-READ-EXIT
     END-IF.
*納品日範囲チェック
 SHTDENLA-READ-3.
     IF       PARA-IN-ONL-NOUHIN-FR   >    DEN-F112
*TEST↓
*             DISPLAY "NOUS-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              GO                  TO   SHTDENLA-READ-SEC
     END-IF.
     IF       PARA-IN-ONL-NOUHIN-TO   <    DEN-F112
*TEST↓
*             DISPLAY "NOUT-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              GO                  TO   SHTDENLA-READ-SEC
     END-IF.
*店舗範囲チェック
 SHTDENLA-READ-4.
     IF       PARA-IN-ONL-TENPO-FR    >    DEN-F07
*TEST↓
*             DISPLAY "TENS-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              GO                  TO   SHTDENLA-READ-SEC
     END-IF.
     IF       PARA-IN-ONL-TENPO-TO    <    DEN-F07
*TEST↓
*             DISPLAY "TENT-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
              GO                  TO   SHTDENLA-READ-SEC
     END-IF.
*物流連携ＦＬＧチェック
 SHTDENLA-READ-5.
*  新規：未連携が対象
*  再送：連携済が対象
     IF       PARA-IN-SOUSIN-KB       =   " "
        IF       DEN-F68                 =   "1"
*TEST↓
*             DISPLAY "FLG -CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
                 GO                  TO   SHTDENLA-READ-SEC
        END-IF
     ELSE
        IF       DEN-F68             NOT =   "1"
*TEST↓
*             DISPLAY "FLG1-CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
                 GO                  TO   SHTDENLA-READ-SEC
        END-IF
     END-IF.
*商品変換ＴＢＬ取得
 SHTDENLA-READ-6.
     PERFORM    SHOTBL1-READ-SEC.
     IF         SHOTBL1-INV-FLG   NOT =  SPACE
*TEST↓
*             DISPLAY "TBL -CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
                GO            TO         SHTDENLA-READ-SEC
     END-IF.
*商品名称マスタ取得
 SHTDENLA-READ-7.
     PERFORM    MEIMS1-READ-SEC.
     IF         MEIMS1-INV-FLG    NOT =  SPACE
*TEST↓
*             DISPLAY "MEI -CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
                GO            TO         SHTDENLA-READ-SEC
     END-IF.
*ＢＭＳ発注ＭＳＧ取得
 SHTDENLA-READ-8.
     PERFORM    BMSHACL1-READ-SEC.
     IF         BMSHACL1-INV-FLG    NOT =  SPACE
*TEST↓
*             DISPLAY "HAC -CHK"     UPON CONS
*             DISPLAY "DEN=" DEN-F02 UPON CONS
*TEST↑
                GO            TO         SHTDENLA-READ-SEC
     END-IF.
*
 SHTDENLA-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 MEIMS1-READ-SEC            SECTION.
*
     MOVE     DEN-F1411     TO         MEI-F011.
     MOVE     DEN-F1412     TO         MEI-F012.
     READ     MEIMS1
              INVALID       MOVE  "INV"   TO  MEIMS1-INV-FLG
*I--------------------------MOVE  ?????   TO  TANE-SIZAI 種資材
*I--------------------------MOVE  ?????   TO  MEI-F89  束区分
*I--------------------------MOVE  1       TO  MEI-F07  入数
                            DISPLAY  NC"商品名称マスタ未登録！"
                                " !" NC"商Ｃ＝" DEN-F141
                                 "!" NC"伝Ｎ＝" DEN-F02
                                 "!" NC"行＝" DEN-F03
***                         MOVE  4000    TO  PROGRAM-STATUS
***                         MOVE  "END"   TO  END-FLG
                            GO            TO  MEIMS1-READ-EXIT
              NOT INVALID   MOVE  SPACE   TO  MEIMS1-INV-FLG
     END-READ.
*
     MOVE     " "   TO  TANE-SIZAI.
     IF   MEIMS1-INV-FLG = SPACE
          MOVE   "2"   TO  TANE-SIZAI
*#2022/03/14 NAV ST
**********IF  MEI-F09 = "01" OR "02" OR "03" OR "04" OR
*                       "05" OR "06" OR "07" OR "08"
*             MOVE   "1"   TO  TANE-SIZAI
*             GO     TO    MEIMS1-READ-EXIT
*         END-IF
*         IF  MEI-F09 = "13" OR "14"
*             MOVE   "2"   TO  TANE-SIZAI
*             GO     TO    MEIMS1-READ-EXIT
**********END-IF
          IF  MEI-F09 = "01" OR "02" OR "03"
              MOVE   "1"   TO  TANE-SIZAI
              GO     TO    MEIMS1-READ-EXIT
          END-IF
          IF  MEI-F09 = "05"
              MOVE   "2"   TO  TANE-SIZAI
              GO     TO    MEIMS1-READ-EXIT
          END-IF
*#2022/03/14 NAV ED
     END-IF.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 SHOTBL1-READ-SEC        SECTION.
*
     MOVE  DEN-F01               TO  SHO-F01.
     MOVE  DEN-F25               TO  SHO-F02.
     READ  SHOTBL1
       INVALID
         DISPLAY  NC"商品変換ＴＢＬ未登録！"
             " !" NC"取Ｃ＝" DEN-F01
              "!" NC"相手商Ｃ＝" DEN-F25
         MOVE  "INV"             TO  SHOTBL1-INV-FLG
***      MOVE  4000              TO  PROGRAM-STATUS
***      MOVE  "END"             TO  END-FLG
         GO                      TO  SHOTBL1-READ-EXIT
*I-------MOVE  "0"               TO  SHO-F10 ラベル張替区分
       NOT INVALID
         MOVE  SPACE             TO  SHOTBL1-INV-FLG
     END-READ.
     IF  DEN-F1411 = SPACE
         MOVE  SHO-F031          TO  DEN-F1411
         MOVE  SHO-F032          TO  DEN-F1412
     END-IF.
 SHOTBL1-READ-EXIT.
     EXIT.
****************************************************************
*    ＢＭＳ発注ＭＳＧ検索                                      *
****************************************************************
 BMSHACL1-READ-SEC        SECTION.
*
     MOVE  DEN-F46               TO  WK-BACHI-NO(1:8).
     MOVE  DEN-F47               TO  WK-BACHI-NO(9:4).
     MOVE  DEN-F01               TO  WK-BACHI-NO(13:8).
*   受信日
     MOVE  WK-BACHI-NO-1         TO  HAC-F011.
*   受信時刻
     MOVE  WK-BACHI-NO-2         TO  HAC-F012.
*   取引先ＣＤ
     MOVE  WK-BACHI-NO-3         TO  HAC-F013.
*   出荷場所（振分倉庫）
     MOVE  DEN-F48               TO  HAC-F02.
*   最終納品先ＣＤ（店舗ＣＤ）
     MOVE  DEN-F07               TO  HAC-F308.
*   最終納品先納品日（納品日）
     MOVE  DEN-F112              TO  HAC-F346.
*   取引番号（伝票番号）
     MOVE  DEN-F02               TO  HAC-F302.
*   取引明細番号（行番号）
     MOVE  DEN-F03               TO  HAC-F402.
*
     READ  BMSHACL1
       INVALID
         DISPLAY  NC"流通ＢＭＳ発注ＭＳＧ未登録！"
             " !" NC"受信日　＝" DEN-F46
              "!" NC"受信時刻＝" DEN-F47
              "!" NC"取引先　＝" DEN-F01
              "!" NC"出荷場所＝" DEN-F48
              "!" NC"店舗ＣＤ＝" DEN-F07
              "!" NC"納品日　＝" DEN-F112
              "!" NC"伝票番号＝" DEN-F02
              "!" NC"行番号　＝" DEN-F03
         MOVE  "INV"             TO  BMSHACL1-INV-FLG
***      MOVE  4000              TO  PROGRAM-STATUS
***      MOVE  "END"             TO  END-FLG
         GO                      TO  BMSHACL1-READ-EXIT
       NOT INVALID
         MOVE  SPACE             TO  BMSHACL1-INV-FLG
     END-READ.
 BMSHACL1-READ-EXIT.
     EXIT.
****************************************************************
*    欠品明細情報マスタ検索                                    *
****************************************************************
 MSTKMEL1-READ-SEC        SECTION.
*
*   取引先ＣＤ
     MOVE  WK-BACHI-NO-3         TO  KME-F01.
*   欠品区分
     MOVE  DEN-F411              TO  KME-F02.
*
     READ  MSTKMEL1
       INVALID
*        DISPLAY  NC"欠品明細情報マスタ未登録！"
*            " !" NC"取引先ＣＤ＝" WK-BACHI-NO-3
*             "!" NC"欠品区分　＝" DEN-F411
         MOVE  "INV"             TO  MSTKMEL1-INV-FLG
         GO                      TO  MSTKMEL1-READ-EXIT
       NOT INVALID
         MOVE  SPACE             TO  MSTKMEL1-INV-FLG
     END-READ.
 MSTKMEL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
