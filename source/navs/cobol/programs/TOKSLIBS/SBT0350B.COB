****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ抽出                *
*                        ：　　（カインズ手書き）　　　　　　  *
*    作成日／更新日　　　：　2014/07/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを売上伝票データファイル*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2022/03/14 NAV TAKAHASHI          *
*    変更概要　　　　　　：　２０分類変更に伴う改修　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0350B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/07/25.
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
     SELECT   SHTDENLF  ASSIGN    TO        DA-01-VI-SHTDENLF
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F01   DEN-F08
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
                        FILE      STATUS    IS   MEI-ST.
*商品変換ＴＢＬ
     SELECT  SHOTBL1    ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01 SHO-F02
                        FILE      STATUS    IS  SHO-ST.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    IS  TEN-ST.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE      STATUS    IS  JYO-ST.
*カインズＤＰＴマスタ
     SELECT   CNZDPTL1  ASSIGN    TO        DA-01-VI-CNZDPTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DPT-F01
                        FILE      STATUS    IS  DPT-ST.
*カインズカテゴリマスタ
     SELECT   CNZCTEL1  ASSIGN    TO        DA-01-VI-CNZCTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       CTE-F01
                        FILE      STATUS    IS  CTE-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLF
                        LABEL RECORD   IS   STANDARD.
*    2013/02/19↓変更
*****COPY     SHTDENLF   OF        XFDLIB
     COPY     JTCDENF   OF        XFDLIB
*    2013/02/19↑変更
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷連携データ
******************************************************************
 FD  CNZSYKF            BLOCK     CONTAINS    1  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     CNZSYKF   OF        XFDLIB
              JOINING   SYU       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    商品変換ＴＢＬ
******************************************************************
 FD  SHOTBL1             LABEL RECORD   IS   STANDARD.
     COPY     SHOTBL1    OF       XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    カインズＤＰＴマスタ
******************************************************************
 FD  CNZDPTL1           LABEL RECORD   IS   STANDARD.
     COPY     CNZDPTL1  OF        XFDLIB
              JOINING   DPT       PREFIX.
******************************************************************
*    カインズカテゴリマスタ
******************************************************************
 FD  CNZCTEL1           LABEL RECORD   IS   STANDARD.
     COPY     CNZCTEL1  OF        XFDLIB
              JOINING   CTE       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JYOKEN1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  CNZDPTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  CNZCTEL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  999999999.99.
 01  WK-JYO-F04.
     03  WK-JYO-F04-5  PIC  9(05).
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  DEN-ST        PIC  X(02).
     03  SYU-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
     03  SHO-ST        PIC  X(02).
     03  TEN-ST        PIC  X(02).
     03  JYO-ST        PIC  X(02).
     03  DPT-ST        PIC  X(02).
     03  CTE-ST        PIC  X(02).
*バッチ_
 01  WK-BACHI-NO           PIC  X(20).
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
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  JYO-ERR           PIC  N(20)  VALUE
         NC"条件ファイルエラー".
     03  DPT-ERR           PIC  N(20)  VALUE
         NC"カインズＤＰＴマスタエラー".
     03  CTE-ERR           PIC  N(20)  VALUE
         NC"カインズカテゴリマスタエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0350B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0350B".
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
     USE         AFTER     EXCEPTION PROCEDURE SHTDENLF.
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
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TENMS1.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DPT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZDPTL1.
     DISPLAY     DPT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DPT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CTE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZCTEL1.
     DISPLAY     CTE-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CTE-ST    UPON      CONS.
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
     OPEN     I-O       SHTDENLF.
     OPEN     INPUT     MEIMS1.
     OPEN     INPUT     SHOTBL1.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     JYOKEN1.
     OPEN     INPUT     CNZDPTL1.
     OPEN     INPUT     CNZCTEL1.
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
*売上伝票ファイルスタート
     MOVE     SPACE                 TO   DEN-REC.
     INITIALIZE                          DEN-REC.
     MOVE     0                     TO   DEN-F277.
     MOVE     0                     TO   DEN-F274.
     MOVE     PARA-IN-TEG-TOR       TO   DEN-F01.
     MOVE     PARA-IN-SOUKO         TO   DEN-F08.
     MOVE     PARA-IN-TEG-DEN-FR    TO   DEN-F02.
     START    SHTDENLF KEY  >=    DEN-F277  DEN-F274
                                  DEN-F01   DEN-F08
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*売上伝票ファイル読込
     PERFORM  SHTDENLF-READ-SEC.
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
*　バッチ_－受信日(←抽出実行日）
     MOVE     SYS-DATEW           TO   SYU-F011.
*  バッチ_－受信時刻(←抽出実行時刻）
     MOVE     SYS-TIME(1:4)       TO   SYU-F012.
*  バッチ_－取引先コード
     MOVE     PARA-IN-TEG-TOR     TO   SYU-F013.
*  出荷場所
     MOVE     DEN-F08             TO   SYU-F02.
*  欠品区分
     MOVE     "00"                TO   SYU-F91.
*  ONL手書区分
     MOVE     "1"                 TO   SYU-F92.
*  センター店舗直送区分 (分類4桁目を判定)
     IF       DEN-F12(4:1) = " "
*             店直
              MOVE     "1"        TO   SYU-F93
     ELSE
*             センター
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
     MOVE     PARA-IN-BUMONCD     TO   SYU-F98.
*  受信者担当者コード
     MOVE     PARA-IN-TANCD       TO   SYU-F99.
*
*【メッセージヘッダ部】レコード区分"A"--------------------------
*  レコード区分
     MOVE     "A"                 TO   SYU-F101.
*  ヘッダバージョン
     MOVE     "1.3"               TO   SYU-F102.
*  送信者ＩＤ
     MOVE     "89580600"          TO   SYU-F103.
*  送信者ＩＤ発行元
     MOVE     "CODE"              TO   SYU-F104.
*  受信者ＩＤ
     MOVE     "92108400"          TO   SYU-F105.
*  受信者ＩＤ発行元
     MOVE     "CODE"              TO   SYU-F106.
*  流通ＢＭＳ名称
     MOVE     "SECONDGENEDI"      TO   SYU-F107.
*  バージョン
     MOVE     "1P"                TO   SYU-F108.
*  インスタンスＩＤ
     MOVE     SPACE               TO   SYU-F109.
*  メッセージ種
     MOVE     "ORDER"             TO   SYU-F110.
*  複合メッセージフラグ
     MOVE     SPACE               TO   SYU-F111.
*  作成日時
     MOVE     SPACE               TO   SYU-F112.
*----MOVE     SYS-DATEW           TO   SYU-F112(1:8).
*----MOVE     SYS-TIME(1:6)       TO   SYU-F112(9:6).
*  テスト区分タイプ
     MOVE     SPACE               TO   SYU-F113.
*  テスト区分インスタンスＩＤ
     MOVE     SPACE               TO   SYU-F114.
*  テスト区分ＩＤ
     MOVE     "0"                 TO   SYU-F115.
*  最終送信先タイプ
     MOVE     SPACE               TO   SYU-F116.
*  最終送信先インスタンスＩＤ
     MOVE     SPACE               TO   SYU-F117.
*  最終送信先ＩＤ
     MOVE     "92108400"          TO   SYU-F118.
*  メッセージ識別ＩＤ
     MOVE     SPACE               TO   SYU-F119.
*  送信者ステーションアドレス
     MOVE     "89580600"          TO   SYU-F120.
*  最終受信者ステーションアドレス
     MOVE     "92108400"          TO   SYU-F121.
*  直接受信者ステーションアドレス
     MOVE     "92108400"          TO   SYU-F122.
*  取引件数
     MOVE     ZERO                TO   SYU-F123.
*
*【発注リスト部】レコード区分"B"--------------------------------
*  レコード区分
     MOVE     "B"                 TO   SYU-F201.
*  ＸＭＬ内容バージョンＩＤ
     MOVE     SPACE               TO   SYU-F202.
*  ＸＭＬ構造バージョンＩＤ
     MOVE     SPACE               TO   SYU-F203.
*  拡張情報ネームスペース
     MOVE     SPACE               TO   SYU-F204.
*  拡張情報バージョン番号
     MOVE     SPACE               TO   SYU-F205.
*  支払法人コード
     MOVE     "0024"              TO   SYU-F206.
*  支払法人ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F207.
*  支払法人名称
     MOVE     X"28"               TO   SYU-F2081.
     MOVE     ALL NC"　"          TO   SYU-F208.
     MOVE     X"29"               TO   SYU-F2082.
*  支払法人名称カナ
     MOVE     SPACE               TO   SYU-F209.
*  発注者コード
     MOVE     "0024"              TO   SYU-F210.
*  発注者ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F211.
*  発注者名称
     MOVE     X"28"               TO   SYU-F2121.
     MOVE     NC"（株）カインズ"  TO   SYU-F212.
     MOVE     X"29"               TO   SYU-F2122.
*  発注者名称カナ
     MOVE     "K.K ｶｲﾝｽﾞ"         TO   SYU-F213.
*
*【取引部】レコード区分"C"--------------------------------------
*  レコード区分
     MOVE     "C"                 TO   SYU-F301.
*  取引番号（発注・返品）
     MOVE     DEN-F02             TO   SYU-F302.
*  取引付属番号
     MOVE     "0"                 TO   SYU-F303.
*  直接納品先コード (分類4桁目を判定)
     IF       DEN-F12(4:1) = " "
*             店直の場合は店CD
              MOVE     DEN-F07    TO   SYU-F304
     ELSE
*             センターの場合は条件ファイルより所属センターCD
              MOVE     JYO-F04    TO   WK-JYO-F04-5
              MOVE     WK-JYO-F04 TO   SYU-F304
*             IF       JYOKEN1-INV-FLG  =  SPACE
*                      MOVE       JYO-F04    TO   SYU-F304
*             ELSE
*                      GO         TO   MAIN-99
*             END-IF
     END-IF.
*  直接納品先ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F305.
*  直接納品先名称 (分類4桁目を判定)
     MOVE     X"28"               TO   SYU-F3061.
     IF       DEN-F12(4:1) = " "
*             店直の場合は店名
              MOVE    TEN-F02     TO   SYU-F306
     ELSE
*             センターの場合は条件ファイルより所属センター名
              MOVE    JYO-F03     TO   SYU-F306
     END-IF.
     MOVE     X"29"               TO   SYU-F3062.
*  直接納品先名称カナ (分類4桁目を判定)
     IF       DEN-F12(4:1) = " "
*             店直の場合は店名
              MOVE    TEN-F04     TO   SYU-F307
     ELSE
*             センターの場合は条件ファイルより所属センター名
              MOVE    JYO-F14     TO   SYU-F307
     END-IF.
*  最終納品先コード
     MOVE     DEN-F07             TO   SYU-F308.
*  最終納品先ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F309.
*  最終納品先名称(店舗略名）
     MOVE     X"28"               TO   SYU-F3101.
     MOVE     TEN-F03             TO   SYU-F310.
     MOVE     X"29"               TO   SYU-F3102.
*  最終納品先名称カナ
     MOVE     TEN-F04             TO   SYU-F311.
*  計上部署コード
     MOVE     DEN-F07             TO   SYU-F312.
*  計上部署ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F313.
*  計上部署名称
     MOVE     X"28"               TO   SYU-F3141.
     MOVE     ALL NC"　"          TO   SYU-F314.
     MOVE     X"29"               TO   SYU-F3142.
*  計上部署名称カナ
     MOVE     SPACE               TO   SYU-F315.
*  陳列場所コード
     MOVE     SPACE               TO   SYU-F316.
*  陳列場所名称
     MOVE     X"28"               TO   SYU-F3171.
     MOVE     ALL NC"　"          TO   SYU-F317.
     MOVE     X"29"               TO   SYU-F3172.
*  陳列場所名称カナ
     MOVE     SPACE               TO   SYU-F318.
*  請求取引先コード
     MOVE     "921084"            TO   SYU-F319.
*  請求取引先ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F320.
*  請求取引先名
     MOVE     X"28"               TO   SYU-F3211.
     MOVE     NC"株式会社　サカタのタネ"   TO   SYU-F321.
     MOVE     X"29"               TO   SYU-F3212.
*  請求取引先名カナ
     MOVE     "K.K ｻｶﾀﾉﾀﾈ"        TO   SYU-F322.
*  取引先コード
     MOVE     "921084"            TO   SYU-F323.
*  取引先ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F324.
*  取引先名称
     MOVE     X"28"               TO   SYU-F3251.
     MOVE     NC"（株）サカタのタネ"   TO   SYU-F325.
     MOVE     X"29"               TO   SYU-F3252.
*  取引先名称カナ
     MOVE     "K.K ｻｶﾀﾉﾀﾈ"        TO   SYU-F326.
*  枝番
     MOVE     SPACE               TO   SYU-F327.
*  出荷先コード
     MOVE     SPACE               TO   SYU-F328.
*  出荷場所ＧＬＮ
     MOVE     "0000000000000"     TO   SYU-F329.
*  納品経路   (分類4桁目を判定)
     IF       DEN-F12(4:1) = " "
*             店直
              MOVE     "01"       TO   SYU-F330
     ELSE
*             センター
              MOVE     "02"       TO   SYU-F330
     END-IF.
*  便NO
     MOVE     "00"                TO   SYU-F331.
*  通過在庫区分
     MOVE     SPACE               TO   SYU-F332.
*  納品区分
     MOVE     "00"                TO   SYU-F333.
*  指定納品時刻
     MOVE     SPACE               TO   SYU-F334.
*  輸送手段
     MOVE     SPACE               TO   SYU-F335.
*  バーコード情報
     MOVE     SPACE               TO   SYU-F336.
*  カテゴリ名称１（印字用）
     MOVE     X"28"               TO   SYU-F3371.
     MOVE     ALL NC"　"          TO   SYU-F337.
     MOVE     X"29"               TO   SYU-F3372.
*  カテゴリ名称２（印字用）
     MOVE     X"28"               TO   SYU-F3381.
     MOVE     ALL NC"　"          TO   SYU-F338.
     MOVE     X"29"               TO   SYU-F3382.
*  最終納品先名称（印字用）
     MOVE     X"28"               TO   SYU-F3391.
     MOVE     ALL NC"　"          TO   SYU-F339.
     MOVE     X"29"               TO   SYU-F3392.
*  ラベル自由使用欄（印字用）
     MOVE     X"28"               TO   SYU-F3401.
     MOVE     ALL NC"　"          TO   SYU-F340.
     MOVE     X"29"               TO   SYU-F3402.
*  ラベル自由使用欄半角カナ（印字用）
     MOVE     SPACE               TO   SYU-F341.
*  商品分類（大）     DPTｺｰﾄﾞ
     MOVE     DEN-F12(1:3)        TO   SYU-F342.
*  商品分類（中）
     MOVE     SPACE               TO   SYU-F343.
*  発注日
     MOVE     DEN-F111            TO   SYU-F344.
*  直接納品先納品日  (分類4桁目を判定)
     IF       DEN-F12(4:1) = " "
*             店直
              MOVE     DEN-F112   TO   SYU-F345
     ELSE
*             センター
*           *******************
*           * 納品日 の 1日前 *
*           *******************
              MOVE    "6"        TO        LINK-IN-KBN
              MOVE     000001    TO        LINK-IN-YMD6
              MOVE     DEN-F112  TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
              IF       LINK-OUT-RET   =    ZERO
                       MOVE      LINK-OUT-YMD8  TO   SYU-F345
              ELSE
                       MOVE      ZERO           TO   SYU-F345
              END-IF
*
     END-IF.
*  最終納品先納品日
     MOVE     DEN-F112            TO   SYU-F346.
*  計上日
     MOVE     ZERO                TO   SYU-F347.
*  販促開始日
     MOVE     ZERO                TO   SYU-F348.
*  販促終了日
     MOVE     ZERO                TO   SYU-F349.
*  取引（発注・返品）データ有効日
     MOVE     ZERO                TO   SYU-F350.
*  商品区分 (商区1桁目を判定)
     IF       DEN-F131(1:1) = "1"
              MOVE     "01"       TO   SYU-F351
     END-IF.
     IF       DEN-F131(1:1) = "2"
              MOVE     "02"       TO   SYU-F351
     END-IF.
     IF       DEN-F131(1:1) = "3"
              MOVE     "03"       TO   SYU-F351
     END-IF.
*  発注区分 (商区2桁目を判定)
     IF       DEN-F131(2:1) = "1"
              MOVE     "01"       TO   SYU-F352
     END-IF.
     IF       DEN-F131(2:1) = "2"
              MOVE     "02"       TO   SYU-F352
     END-IF.
*  出荷データ有無区分
     MOVE     "02"                TO   SYU-F353.
*  ＰＢ区分
     MOVE     SPACE               TO   SYU-F354.
*  配送温度区分
     MOVE     SPACE               TO   SYU-F355.
*  酒区分
     MOVE     SPACE               TO   SYU-F356.
*  処理種別
     IF       DEN-F132  =         SPACE
              MOVE      "AA"      TO   SYU-F357
     ELSE
              MOVE      DEN-F132  TO   SYU-F357
     END-IF.
*  伝票レス区分
     MOVE     SPACE               TO   SYU-F358.
*  取引番号区分
     MOVE     SPACE               TO   SYU-F359.
*  パック区分
     MOVE     SPACE               TO   SYU-F360.
*  不定貫区分
     MOVE     SPACE               TO   SYU-F361.
*  税区分
     MOVE     "05"                TO   SYU-F362.
*  税率
     MOVE     ZERO                TO   SYU-F363.
*  自由使用欄
     MOVE     X"28"               TO   SYU-F3641.
     MOVE     ALL NC"　"          TO   SYU-F364.
     MOVE     X"29"               TO   SYU-F3642.
*  自由使用欄半角カナ
     MOVE     SPACE               TO   SYU-F365.
*  原価金額合計
     MOVE     ZERO                TO   SYU-F366.
*  売価金額合計
     MOVE     ZERO                TO   SYU-F367.
*  税額金額合計
     MOVE     ZERO                TO   SYU-F368.
*  数量合計
     MOVE     ZERO                TO   SYU-F369.
*  発注単位数量合計
     MOVE     ZERO                TO   SYU-F370.
*  重量合計
     MOVE     ZERO                TO   SYU-F371.
*
*【取引明細部】レコード区分"D"----------------------------------
*  レコード区分
     MOVE     "D"                 TO   SYU-F401.
*  取引明細番号（発注・返品）
     MOVE     DEN-F03             TO   SYU-F402.
*  取引付属明細番号 (追加区分？)
     MOVE     SPACE               TO   SYU-F403.
*  元取引番号
     MOVE     SPACE               TO   SYU-F404.
*  元取引明細番号
     MOVE     SPACE               TO   SYU-F405.
*  商品分類（小） （←カテゴリＣＤ）
     MOVE     CTE-F01             TO   SYU-F406.
*  商品分類（細） （外注NO ？）
     MOVE     SPACE               TO   SYU-F407.
*----MOVE     DEN-F10             TO   SYU-F407.
*  配達予定日
     MOVE     ZERO                TO   SYU-F408.
*  納品期限
     MOVE     ZERO                TO   SYU-F409.
*  センター納品詳細指示
     MOVE     SPACE               TO   SYU-F410.
*  メーカーコード
     MOVE     SPACE               TO   SYU-F411.
*  商品コード（ＧＴＩＮ）
     MOVE     "0"                 TO   SYU-F412(1:1).
     MOVE     DEN-F25             TO   SYU-F412(2:13).
*  商品コード（発注用）
     MOVE     "0000000"           TO   SYU-F413.
*  商品コード区分
     MOVE     SPACE               TO   SYU-F414.
*----MOVE     "999"               TO   SYU-F414.
*  商品コード（取引先）
     MOVE     SPACE               TO   SYU-F415.
*----MOVE     "00000"             TO   SYU-F415.
*  商品名
     MOVE     X"28"               TO   SYU-F4161.
     MOVE     MEI-F02             TO   SYU-F416.
     MOVE     X"29"               TO   SYU-F4162.
*  商品名カナ
     MOVE     DEN-F1421           TO   SYU-F417(1:15).
     MOVE     DEN-F1422           TO   SYU-F417(16:10).
*----MOVE     MEI-F03             TO   SYU-F417.
*  規格  (カテゴリ名)
     MOVE     X"28"               TO   SYU-F4181.
     MOVE     CTE-F02             TO   SYU-F418.
     MOVE     X"29"               TO   SYU-F4182.
*  規格カナ
     MOVE     SPACE               TO   SYU-F419.
*  カラーコード
     MOVE     "0000"              TO   SYU-F420.
*  カラー名称
     MOVE     X"28"               TO   SYU-F4211.
     MOVE     ALL NC"　"          TO   SYU-F421.
     MOVE     X"29"               TO   SYU-F4212.
*  カラー名称カナ
     MOVE     SPACE               TO   SYU-F422.
*  サイズコード
     MOVE     "0000"              TO   SYU-F423.
*  サイズ名称
     MOVE     X"28"               TO   SYU-F4241.
     MOVE     ALL NC"　"          TO   SYU-F424.
     MOVE     X"29"               TO   SYU-F4242.
*  サイズ名称カナ
     MOVE     SPACE               TO   SYU-F425.
*  入数
     MOVE     ZERO                TO   SYU-F426.
*  都道府県コード
     MOVE     SPACE               TO   SYU-F427.
*  国コード
     MOVE     SPACE               TO   SYU-F428.
*  産地名
     MOVE     X"28"               TO   SYU-F4291.
     MOVE     ALL NC"　"          TO   SYU-F429.
     MOVE     X"29"               TO   SYU-F4292.
*  水域コード
     MOVE     SPACE               TO   SYU-F430.
*  水域名
     MOVE     X"28"               TO   SYU-F4311.
     MOVE     ALL NC"　"          TO   SYU-F431.
     MOVE     X"29"               TO   SYU-F4312.
*  原産エリア
     MOVE     X"28"               TO   SYU-F4321.
     MOVE     ALL NC"　"          TO   SYU-F432.
     MOVE     X"29"               TO   SYU-F4322.
*  等級
     MOVE     X"28"               TO   SYU-F4331.
     MOVE     ALL NC"　"          TO   SYU-F433.
     MOVE     X"29"               TO   SYU-F4332.
*  階級
     MOVE     X"28"               TO   SYU-F4341.
     MOVE     ALL NC"　"          TO   SYU-F434.
     MOVE     X"29"               TO   SYU-F4342.
*  銘柄
     MOVE     X"28"               TO   SYU-F4351.
     MOVE     ALL NC"　"          TO   SYU-F435.
     MOVE     X"29"               TO   SYU-F4352.
*  商品ＰＲ
     MOVE     X"28"               TO   SYU-F4361.
     MOVE     ALL NC"　"          TO   SYU-F436.
     MOVE     X"29"               TO   SYU-F4362.
*  バイオ区分
     MOVE     SPACE               TO   SYU-F437.
*  品種コード
     MOVE     SPACE               TO   SYU-F438.
*  養殖区分
     MOVE     SPACE               TO   SYU-F439.
*  解凍区分
     MOVE     SPACE               TO   SYU-F440.
*  商品状態区分
     MOVE     SPACE               TO   SYU-F441.
*  形状・部位
     MOVE     X"28"               TO   SYU-F4421.
     MOVE     ALL NC"　"          TO   SYU-F442.
     MOVE     X"29"               TO   SYU-F4422.
*  用途
     MOVE     X"28"               TO   SYU-F4431.
     MOVE     ALL NC"　"          TO   SYU-F443.
     MOVE     X"29"               TO   SYU-F4432.
*  決定管理義務商材区分
     MOVE     SPACE               TO   SYU-F444.
*  原価金額
     MOVE     DEN-F181            TO   SYU-F445.
*  原単価
     MOVE     DEN-F172            TO   SYU-F446.
*  売価金額
     MOVE     DEN-F182            TO   SYU-F447.
*  売単価
     MOVE     DEN-F173            TO   SYU-F448.
*  税額
     MOVE     ZERO                TO   SYU-F449.
*  発注単位   名称マスタの入数
     MOVE     MEI-F07             TO   SYU-F450.
*??発注単位コード  詳細不明？？？？
     MOVE     SPACE               TO   SYU-F451.
*----MOVE     "??"                TO   SYU-F451.
*??発注荷姿コード  詳細不明？？？？
     MOVE     "01"                TO   SYU-F452.
*??  IF       ???-F???  =  ???  （バラ）
*??           MOVE     "01"       TO   SYU-F452
*??  END-IF.
*??  IF       ???-F???  =  ???  （ボール）
*??           MOVE     "02"       TO   SYU-F452
*??  END-IF.
*??  IF       ???-F???  =  ???  （ケース）
*??           MOVE     "03"       TO   SYU-F452
*??  END-IF.
*  発注数量（バラ）
     MOVE     DEN-F15             TO   SYU-F453.
*  発注数量（発注単位数）束区分考慮
     IF       MEI-F89   =   "1"
              DIVIDE  DEN-F15  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI   NOT = 0
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     MOVE    DEN-F15    TO  WK-SUURYOU
                     DISPLAY NC"数量が入数で割り切れません！"
                       " !"  NC"商Ｃ＝" MEI-F01
                        "!"  NC"入数＝" WK-MEI-F07
                        "!"  NC"数量＝" WK-SUURYOU
                        "!"  NC"取Ｃ＝" DEN-F01
                        "!"  NC"店Ｃ＝" DEN-F07
                        "!"  NC"納日＝" DEN-F112
                        "!"  NC"伝票＝" DEN-F02
                        "!"  NC"行＝" DEN-F03
                      GO       TO             MAIN-99
              END-IF
     ELSE
              MOVE    DEN-F15             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F454.
*  取引単位重量
     MOVE     ZERO                TO   SYU-F455.
*  単価登録単位
     MOVE     SPACE               TO   SYU-F456.
*  商品重量
     MOVE     ZERO                TO   SYU-F457.
*  発注重量
     MOVE     ZERO                TO   SYU-F458.
*
*注）以降の項目は、必要に応じて個別にセットする項目である。
*【出荷関連情報－取引】レコード区分"C"--------------------------
*  出荷者管理番号
     MOVE     SPACE               TO   SYU-F501.
*  入荷管理用メーカーコード
     MOVE     SPACE               TO   SYU-F502.
*  センター納品書番号
     MOVE     SPACE               TO   SYU-F503.
*  ＥＯＳ区分
     MOVE     SPACE               TO   SYU-F504.
*
*【出荷関連情報－取引明細】レコード区分"D"----------------------
*  出荷者管理明細番号
     MOVE     SPACE               TO   SYU-F601.
*  仮伝フラグ
     MOVE     SPACE               TO   SYU-F602.
*  商品コード（出荷元）
     MOVE     SPACE               TO   SYU-F603.
*  出荷数量（バラ）
     MOVE     DEN-F15             TO   SYU-F604.
*  出荷数量（発注単位数）  発注数量（発注単位数）と同値
*****MOVE     ZERO                TO   SYU-F605.
     MOVE     SYU-F454            TO   SYU-F605.
*  欠品数量（バラ）
     MOVE     ZERO                TO   SYU-F606.
*  欠品数量（発注単位数）
     MOVE     ZERO                TO   SYU-F607.
*  欠品区分   共通部の欠品区分と同一
     MOVE     SYU-F91             TO   SYU-F608.
*  出荷重量
     MOVE     ZERO                TO   SYU-F609.
*
*【出荷関連情報－出荷荷姿情報】レコード区分"E"------------------
*  ＩＴＦコード（集合包装ＧＴＩＮ）
     MOVE     SPACE               TO   SYU-F701.
*  出荷荷姿コード
     MOVE     SPACE               TO   SYU-F702.
*  出荷数量（出荷荷姿数）
     MOVE     ZERO                TO   SYU-F703.
*  賞味期限日
     MOVE     ZERO                TO   SYU-F704.
*  製造日
     MOVE     ZERO                TO   SYU-F705.
*  製造番号
     MOVE     SPACE               TO   SYU-F706.
*
*【出荷関連情報－処理制御】-------------------------------------
*  出荷情報抽出済ＦＬＧ
*             引き渡す項目ではない
*  抽出日
     MOVE     SYS-DATEW           TO   SYU-F802.
*  抽出時刻
     MOVE     SYS-HHMMSS          TO   SYU-F803.
*----------------------------------------------------------------
*2014/08/27 ３６００バイトで送信するため
*           レコード末尾に文字をセットしておく
     MOVE     "1"                 TO   SYU-FIL7(100:1).
*----------------------------------------------------------------
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
     PERFORM  SHTDENLF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*ＰＡＲＡ（ＯＵＴ）セット
     IF  WRT-CNT1 > ZERO
         MOVE "L"                       TO PARA-OUT-SYURUI
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
     CLOSE     SHTDENLF  CNZSYKF  MEIMS1    SHOTBL1
               TENMS1    JYOKEN1  CNZDPTL1  CNZCTEL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENLF-READ-SEC          SECTION.
*
     READ     SHTDENLF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SHTDENLF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*売上データ作成区分（０ブレイクで終了）　
     IF       DEN-F277       =   0
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLF-READ-EXIT
     END-IF.
*オンライン区分（０ブレイクで終了）　
     IF       DEN-F274       =   0
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLF-READ-EXIT
     END-IF.
*取引先チェック（ＰＡＲＡ取引先ブレイクで終了）
     IF       PARA-IN-TEG-TOR =   DEN-F01
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLF-READ-EXIT
     END-IF.
*抽出倉庫チェック（手書き：出荷場所）
     IF       PARA-IN-SOUKO  =   DEN-F08
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLF-READ-EXIT
     END-IF.
*伝票番号チェック（ＰＡＲＡ伝票番号（ＴＯ）ブレイクで終了）
     IF       PARA-IN-TEG-DEN-TO      <    DEN-F02
              MOVE      "END"     TO   END-FLG
              GO                  TO   SHTDENLF-READ-EXIT
     END-IF.
*相殺区分チェック（０以外読み飛ばし）
     IF       DEN-F04   NOT   =   0
              GO                  TO   SHTDENLF-READ-SEC
     END-IF.
*伝票区分チェック（４０以外読み飛ばし）
     IF       DEN-F051  NOT   =   40
              GO                  TO   SHTDENLF-READ-SEC
     END-IF.
*行番号チェック（８０は読み飛ばし）
     IF       DEN-F03         =   80
              GO                  TO   SHTDENLF-READ-SEC
     END-IF.
*納品日範囲チェック（範囲外は読み飛ばし）
     IF       PARA-IN-TEG-NOUHIN-FR   >    DEN-F112
              GO                  TO   SHTDENLF-READ-SEC
     END-IF.
     IF       PARA-IN-TEG-NOUHIN-TO   <    DEN-F112
              GO                  TO   SHTDENLF-READ-SEC
     END-IF.
*物流連携ＦＬＧチェック
*  新規：未連携が対象
*  再送：連携済が対象
     IF       PARA-IN-SOUSIN-KB       =   " "
        IF       DEN-F68                 =   "1"
                 GO                  TO   SHTDENLF-READ-SEC
        END-IF
     ELSE
        IF       DEN-F68             NOT =   "1"
                 GO                  TO   SHTDENLF-READ-SEC
        END-IF
     END-IF.
*出荷数量が０以下の明細は抽出対象としない。
*    IF  DEN-F15  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  SHTDENLF-READ-SEC
*    END-IF.
*商品変換ＴＢＬ取得
     PERFORM    SHOTBL1-READ-SEC.
*商品名称マスタ取得
     PERFORM    MEIMS1-READ-SEC.
     IF         MEIMS1-INV-FLG    NOT =  SPACE
                IF  SHOTBL1-INV-FLG   NOT =  SPACE
                    DISPLAY  NC"商品変換ＴＢＬ未登録！"
                       " !"  NC"取Ｃ＝" DEN-F01
                        "!"  NC"商Ｃ＝" DEN-F25
                END-IF
                GO            TO         SHTDENLF-READ-SEC
     END-IF.
*店舗マスタ取得
     PERFORM    TENMS1-READ-SEC.
     IF         TENMS1-INV-FLG    NOT =  SPACE
                GO            TO         SHTDENLF-READ-SEC
     END-IF.
*条件ファイル取得
*    センター納品のみ
     IF         DEN-F12(4:1)  NOT = " "
                PERFORM    JYOKEN1-READ-SEC
                IF         JYOKEN1-INV-FLG   NOT =  SPACE
                           GO      TO        SHTDENLF-READ-SEC
                END-IF
     END-IF.
*
*カインズＤＰＴマスタ取得
     PERFORM    CNZDPTL1-READ-SEC.
     IF         CNZDPTL1-INV-FLG    NOT =  SPACE
                GO            TO         SHTDENLF-READ-SEC
     END-IF.
*
*カインズカテゴリマスタ取得
     PERFORM    CNZCTEL1-READ-SEC.
     IF         CNZCTEL1-INV-FLG    NOT =  SPACE
                GO            TO         SHTDENLF-READ-SEC
     END-IF.
*
 SHTDENLF-READ-EXIT.
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
                                 "!" NC"相商＝" DEN-F25
*
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
*#2022/03/14 NAV EC
     END-IF.
*
     IF   MEI-F07  = ZERO
          MOVE     1       TO  MEI-F07
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
*2013/03/08 NAV ST 印字しない様に変更
*********DISPLAY  NC"商品変換ＴＢＬ未登録！"
*           " !"  NC"取Ｃ＝" DEN-F01
*********    "!"  NC"商Ｃ＝" DEN-F25       UPON CONS
*2013/03/08 NAV ED 印字しない様に変更
         MOVE  "INV"             TO  SHOTBL1-INV-FLG
***      MOVE  4000              TO  PROGRAM-STATUS
***      MOVE  "END"             TO  END-FLG
*2013/03/08 NAV ST 商品変換テーブルが未登録の場合、ラベル張替
*                  区分に空白をセットする。
         MOVE  SPACE             TO  SHO-F10
*2013/03/08 NAV ED
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
*　　　　　　店舗マスタ読込
****************************************************************
 TENMS1-READ-SEC            SECTION.
*
     MOVE     DEN-F01       TO         TEN-F52.
     MOVE     DEN-F07       TO         TEN-F011.
     READ     TENMS1
              INVALID       MOVE  "INV"   TO  TENMS1-INV-FLG
                            DISPLAY  NC"店舗マスタ未登録！"
                                " !" NC"取引先Ｃ＝" DEN-F01
                                 "!" NC"店舗Ｃ　＝" DEN-F07
*
                            GO            TO  TENMS1-READ-EXIT
              NOT INVALID   MOVE  SPACE   TO  TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　条件ファイル読込
****************************************************************
 JYOKEN1-READ-SEC            SECTION.
*
     MOVE     23            TO         JYO-F01.
     MOVE     TEN-F75       TO         JYO-F02.
     READ     JYOKEN1
              INVALID       MOVE  "INV"   TO  JYOKEN1-INV-FLG
                            DISPLAY  NC"条件ファイル未登録！"
                                " !" NC"キー＝" "23"
                                 "!" NC"センター区分＝" TEN-F76
                                 "(" NC"店舗＝" DEN-F07 " )"
*
                            GO            TO  JYOKEN1-READ-EXIT
              NOT INVALID   MOVE  SPACE   TO  JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　カインズＤＰＴマスタ読込
****************************************************************
 CNZDPTL1-READ-SEC            SECTION.
*
     MOVE  DEN-F12(1:3) TO         DPT-F01.
     READ  CNZDPTL1
           INVALID      MOVE  "INV"   TO  CNZDPTL1-INV-FLG
                        DISPLAY NC"カインズＤＰＴマスタ未登録！"
                           " !" NC"ＤＰＴ　＝" DEN-F12(1:3)
                           " (" NC"伝票ＮＯ＝" DEN-F02 " )"
*
                        GO            TO  CNZDPTL1-READ-EXIT
           NOT INVALID  MOVE  SPACE   TO  CNZDPTL1-INV-FLG
     END-READ.
*
 CNZDPTL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　カインズカテゴリマスタ読込
****************************************************************
 CNZCTEL1-READ-SEC            SECTION.
*
     MOVE  DPT-F03      TO         CTE-F01.
     READ  CNZCTEL1
           INVALID      MOVE  "INV"   TO  CNZCTEL1-INV-FLG
                        DISPLAY
                              NC"カインズカテゴリマスタ未登録！"
                                " !"
                              NC"カテゴリ＝"
                              DPT-F03
                        GO            TO  CNZCTEL1-READ-EXIT
           NOT INVALID  MOVE  SPACE   TO  CNZCTEL1-INV-FLG
     END-READ.
*
 CNZCTEL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
