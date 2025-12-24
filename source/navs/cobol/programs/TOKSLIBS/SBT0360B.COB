****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携累積データ削除　　　　　  *
*                        ：　　（カインズオンライン分）　　　  *
*    作成日／更新日　　　：　2014/07/30                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、　　    *
*                            出荷連携累積データを削除する。　　*
*　　更新日／更新者　　　：　    /  /                          *
*    変更概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0360B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/07/30.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*カインズ出荷連携累積データ
     SELECT   CNZSYRL1  ASSIGN    TO        DA-01-VI-CNZSYRL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SYU-F92
                                                 SYU-F02
                                                 SYU-F011
                                                 SYU-F012
                                                 SYU-F013
                                                 SYU-F346
                                                 SYU-F308
                        FILE      STATUS    IS   SYU-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    カインズ出荷連携累積データ
******************************************************************
 FD  CNZSYRL1           BLOCK     CONTAINS   1   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     CNZSYRL1  OF        XFDLIB
              JOINING   SYU       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-SYU-F112             PIC  9(08)     VALUE  ZERO.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  BMSHACL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  MSTKMEL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-SYU-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-SYU-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  999999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
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
         NC"カインズ出荷連携累積データエラー".
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
         05  ST-PG          PIC   X(08)  VALUE "SBT0360B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0360B".
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
     03  MSG-DEL.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " DELETE = ".
         05  OUT-CNT2       PIC   9(08).
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
*
 DECLARATIVES.
 SYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CNZSYRL1.
     DISPLAY     SYU-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYU-ST    UPON      CONS.
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
     OPEN     I-O       CNZSYRL1.
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
     MOVE     ZERO      TO        IN-CNT    OUT-CNT    DEL-CNT.
*カインズ出荷連携累積データ スタート
     MOVE     SPACE          TO   SYU-REC.
     INITIALIZE                   SYU-REC.
     MOVE     "0"                   TO   SYU-F92.
     MOVE     PARA-IN-SOUKO         TO   SYU-F02.
     MOVE     PARA-IN-ONL-JUSIN-HI  TO   SYU-F011.
     MOVE     PARA-IN-ONL-JUSIN-JI  TO   SYU-F012.
     MOVE     PARA-IN-ONL-JUSIN-TOR TO   SYU-F013.
     MOVE     PARA-IN-ONL-NOUHIN-FR TO   SYU-F346.
     MOVE     PARA-IN-ONL-TENPO-FR  TO   SYU-F308.
     START    CNZSYRL1  KEY  >=   SYU-F92
                                  SYU-F02   SYU-F011
                                  SYU-F012  SYU-F013
                                  SYU-F346  SYU-F308
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*カインズ出荷連携累積データ 読込
     PERFORM  CNZSYRL1-READ-SEC.
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
*
*カインズ出荷連携累積データ 削除
     DELETE   CNZSYRL1.
*対象件数確認
     ADD      1                   TO   DEL-CNT.
*カインズ出荷連携累積データ 読込
 MAIN-99.
     PERFORM  CNZSYRL1-READ-SEC.
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
     MOVE      RD-CNT    TO   IN-CNT.
     MOVE      DEL-CNT   TO   OUT-CNT2.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-DEL   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     CNZSYRL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　カインズ出荷連携累積データ読込
****************************************************************
 CNZSYRL1-READ-SEC          SECTION.
*
     READ     CNZSYRL1  NEXT
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO CNZSYRL1-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" DEL-CNT  ") #"
                      UPON CONS
     END-IF.
*オ／手区分チェック     "0"(オンライン)以外で終了
 CNZSYRL1-READ-0.
     IF       SYU-F92    =       "0"
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   CNZSYRL1-READ-EXIT
     END-IF.
*抽出倉庫チェック       ＰＡＲＡ倉庫ブレイクで終了
 CNZSYRL1-READ-1.
     IF       PARA-IN-SOUKO  =   SYU-F02
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   CNZSYRL1-READ-EXIT
     END-IF.
*指定バッチ_チェック  ＰＡＲＡバッチ_ブレイクで終了
 CNZSYRL1-READ-2.
     IF     ( PARA-IN-ONL-JUSIN-HI  =  SYU-F011 ) AND
            ( PARA-IN-ONL-JUSIN-JI  =  SYU-F012 ) AND
            ( PARA-IN-ONL-JUSIN-TOR =  SYU-F013 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   CNZSYRL1-READ-EXIT
     END-IF.
*納品日範囲チェック    ＰＡＲＡ納品日（ＴＯ）超で終了
 CNZSYRL1-READ-3.
     IF       PARA-IN-ONL-NOUHIN-TO   <    SYU-F346
              MOVE      "END"     TO   END-FLG
              GO                  TO   CNZSYRL1-READ-EXIT
     END-IF.
* 店舗範囲チェック     範囲外は読み飛ばし
*?       左詰かどうか？　５桁かどうか？
 CNZSYRL1-READ-4.
     IF       PARA-IN-ONL-TENPO-FR    >    SYU-F308(1:5)
              GO                  TO   CNZSYRL1-READ-SEC
     END-IF.
     IF       PARA-IN-ONL-TENPO-TO    <    SYU-F308(1:5)
              GO                  TO   CNZSYRL1-READ-SEC
     END-IF.
*
 CNZSYRL1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
