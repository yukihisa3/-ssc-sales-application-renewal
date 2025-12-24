****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　入荷連携データ抽出（入荷）　　　　*
*    作成日／更新日　　　：　2012/10/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを入庫ファイル　　　　　*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2013/03/19 NAV TAKAHASHI          *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　商品変換ＴＢＬ索引を追加          *
*      　　　　　　　　　　　張替区分＝”２”の時、相手商品ＣＤ*
*      　　　　　　　　　　　をセットする。　　　　　　　　　　*
*　　更新日／更新者　　　：　2013/08/16  NAV TAKAHASHI         *
*    修正概要　　　　　　：　担当者ＣＤ＝０８、１８以外は連携  *
*      　　　　　　　　　　　しない様に変更する。　　　　　　　*
*　　更新日／更新者　　　：　2015/08/03  NAV TAKAHASHI         *
*    修正概要　　　　　　：　担当者ＣＤ＝２９以外は連携しない  *
*      　　　　　　　　　　　様に変更する。　　　　　　　　　　*
*　　更新日／更新者　　　：　2022/10/12  NAV TAKAHASHI         *
*    修正概要　　　　　　：　担当者ＣＤ＝３１、３２を追加　　  *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0170B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*入庫ファイル（新規／再送共用）
     SELECT   NYKFILL3  ASSIGN    TO        DA-01-VI-NYKFILL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NK3-F94   NK3-F27
                                            NK3-F95
                        FILE  STATUS   IS   NK3-ST.
*入荷連携データ
     SELECT   LNKNYKF   ASSIGN    TO        DA-01-S-LNKNYKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   NYU-ST.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    IS   MEI-ST.
*商品変換ＴＢＬ
     SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                                            SHO-F04
                                            SHO-F031
                                            SHO-F0321
                                            SHO-F0322
                                            SHO-F0323
                        FILE      STATUS    IS   SHO-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    入庫ファイル
******************************************************************
 FD  NYKFILL3
                        LABEL RECORD   IS   STANDARD.
     COPY     NYKFILL3   OF        XFDLIB
              JOINING   NK3  AS   PREFIX.
*
******************************************************************
*    入荷連携データ
******************************************************************
 FD  LNKNYKF            BLOCK     CONTAINS   66  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     LNKNYKF  OF        XFDLIB
              JOINING   NYU       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    商品変換ＴＢＬ
******************************************************************
 FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL    OF       XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  99999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  NK3-ST        PIC  X(02).
     03  NYU-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
     03  SHO-ST        PIC  X(02).
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
     03  NK3-ERR           PIC  N(20)  VALUE
         NC"入庫ファイルエラー".
     03  NYU-ERR           PIC  N(20)  VALUE
         NC"入荷連携データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SHO-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0170B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0170B".
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
*    抽出区分（入荷）
 01  PARA-IN-CYUSYUTU-NYU   PIC   X(01).
*    抽出区分（横持）
 01  PARA-IN-CYUSYUTU-YOK   PIC   X(01).
*    抽出倉庫CD
 01  PARA-IN-SOUKO          PIC   X(02).
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
                            PARA-IN-CYUSYUTU-NYU
                            PARA-IN-CYUSYUTU-YOK
                            PARA-IN-SOUKO
                            PARA-IN-YOK-HI
                            PARA-OUT-SYURUI
                            PARA-OUT-KENSUU.
 DECLARATIVES.
 NK3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NYKFILL3.
     DISPLAY     NK3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NK3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 NYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKNYKF.
     DISPLAY     NYU-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NYU-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SHO-ST    UPON      CONS.
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
*
     OPEN     I-O       NYKFILL3.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     EXTEND    LNKNYKF.
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
     INITIALIZE                   WK-BUMON.
*送信区分によりＳＴＡＲＴキー値変化
     IF       PARA-IN-SOUSIN-KB  =  "1"
              GO                    TO   INIT-02
     ELSE
              GO                    TO   INIT-01
     END-IF.
*
 INIT-01.
*入庫ファイルスタート（送信区分＝" "（新規））
     MOVE     SPACE                 TO   NK3-REC.
     INITIALIZE                          NK3-REC.
     MOVE     SPACE                 TO   NK3-F94.
     MOVE     PARA-IN-SOUKO         TO   NK3-F27.
     MOVE     SPACE                 TO   NK3-F95.
     START    NYKFILL3  KEY  >=   NK3-F94   NK3-F27   NK3-F95
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*入庫ファイル読込
     PERFORM  NYKFILL3-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
     GO       TO   INIT-EXIT.
*
 INIT-02.
*入庫ファイルスタート（送信区分＝"1"（再送））
     MOVE     SPACE                 TO   NK3-REC.
     INITIALIZE                          NK3-REC.
     MOVE     "1"                   TO   NK3-F94.
     MOVE     PARA-IN-SOUKO         TO   NK3-F27.
     MOVE     PARA-IN-YOK-HI        TO   NK3-F95.
     START    NYKFILL3  KEY  >=   NK3-F94   NK3-F27   NK3-F95
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*入庫ファイル読込
     PERFORM  NYKFILL3-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
     GO       TO   INIT-EXIT.
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
*レコード初期化
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*入荷区分
     MOVE     "01"                TO   NYU-F01.
*入荷日
     MOVE     NK3-F26             TO   NYU-F02.
*伝票番号　　
     MOVE     NK3-F02             TO   NYU-F03(1:7).
     MOVE     NK3-F03             TO   NYU-F03(8:2).
*入荷元コード
     MOVE     NK3-F27             TO   NYU-F04.
*品名コード
     IF       MEI-F06   =         SPACE
              GO        TO        MAIN-99
     END-IF.
     MOVE     MEI-F06             TO   NYU-F05.
*2013/03/19 NAV ST
*商品変換ＴＢＬを索引して、レコード存在で張替区分＝２の場合
*商品変換ＴＢＬの相手商品ＣＤをセットする。
     IF  HSHOTBL-INV-FLG  =  SPACE
         IF  SHO-F10  =  "2"
             MOVE  SHO-F02        TO   NYU-F05
         END-IF
     END-IF.
*2013/03/19 NAV ED
*数量
     IF       MEI-F89   =   "1"
              DIVIDE  NK3-F18  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI   NOT = 0
                     DISPLAY NC"数量が入数で割り切れません！"
                                                       UPON CONS
                     DISPLAY NC"商品ＣＤ＝" MEI-F01    UPON CONS
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     DISPLAY NC"入り数　＝" WK-MEI-F07 UPON CONS
                     MOVE    NK3-F18    TO  WK-SUURYOU
                     DISPLAY NC"数量　　＝" WK-SUURYOU UPON CONS
                     DISPLAY NC"入荷日　＝" NK3-F26    UPON CONS
                     DISPLAY NC"伝票　　＝" NYU-F03    UPON CONS
                     DISPLAY NC"入荷元　＝" NK3-F27    UPON CONS
*10/11                MOVE     4000       TO  PROGRAM-STATUS
*10/11                MOVE     "END"      TO  END-FLG
*10/11                GO       TO             MAIN-EXIT
                      GO       TO             MAIN-99
              END-IF
     ELSE
              MOVE    NK3-F18             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   NYU-F06.
*入荷連携データ出力
     WRITE    NYU-REC.
*入庫ファイルへ連携済FLG更新
     MOVE     "1"                 TO   NK3-F94.
     MOVE     SYS-DATEW           TO   NK3-F95.
     REWRITE  NK3-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*入庫ファイル読込
 MAIN-99.
     PERFORM  NYKFILL3-READ-SEC.
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
         MOVE "G"                       TO PARA-OUT-SYURUI
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
     CLOSE     NYKFILL3.
     CLOSE     LNKNYKF  HMEIMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　入庫ファイル読込
****************************************************************
 NYKFILL3-READ-SEC          SECTION.
*
     READ     NYKFILL3
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO NYKFILL3-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（ブレイクで終了）
*  新規：未連携が対象
*  再送：連携済が対象
     IF       PARA-IN-SOUSIN-KB       =   " "
        IF       NK3-F94                 =   "1"
                 MOVE      "END"     TO   END-FLG
                 GO                  TO   NYKFILL3-READ-EXIT
        END-IF
     ELSE
        IF       NK3-F94             NOT =   "1"
                 MOVE      "END"     TO   END-FLG
                 GO                  TO   NYKFILL3-READ-EXIT
        END-IF
     END-IF.
*倉庫（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       NK3-F27    NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYKFILL3-READ-EXIT
     END-IF.
*相殺区分チェック（０以外読み飛ばし）
     IF       NK3-F04   NOT   =   0
              GO                  TO   NYKFILL3-READ-SEC
     END-IF.
*伝票区分チェック（５０以外読み飛ばし）
     IF       NK3-F01   NOT   =   50
              GO                  TO   NYKFILL3-READ-SEC
     END-IF.
*指定日付以下は対象外
     IF       NK3-F26         <   20130225
              GO                  TO   NYKFILL3-READ-SEC
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  NK3-F18  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  NYKFILL3-READ-SEC
*    END-IF.
*2013/08/16 NAV ST 担当者ＣＤ＝０８、１８以外は連携しない
*担当者ＣＤ判定（担当者ＣＤ＝０８，１８以外は連携しない）
*2014/12/22 NAV ST 担当者ＣＤ追加　さくら　戸上様、船山様
*****IF       NK3-F33  =  "08"  OR  "18"
*****IF       NK3-F33  =  "08"  OR  "18"  OR  "34"  OR  "35"
*2015/08/03 NAV ST 担当者ＣＤ追加　２９：ＫＡ様
     IF       NK3-F33  = "08" OR "18" OR "34" OR "35" OR "29" OR
*2015/08/03 NAV ST 担当者ＣＤ追加31，３２追加
*************************"28"
                         "28"  OR "31" OR "32"
*2015/08/03 NAV ED 担当者ＣＤ追加31，３２追加
*2015/08/03 NAV ED 担当者ＣＤ追加　２９：ＫＡ様
*2014/12/22 NAV ED
              CONTINUE
     ELSE
              GO                  TO   NYKFILL3-READ-SEC
     END-IF.
*2013/08/16 NAV ED 担当者ＣＤ＝０８、１８以外は連携しない
*商品名称マスタ取得
     MOVE       NK3-F15       TO         MEI-F011.
     MOVE       NK3-F16       TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         NYKFILL3-READ-SEC
     END-IF.
*商品変換ＴＢＬ取得
     MOVE       NK3-F09       TO         SHO-F01.
     MOVE       NK3-F27       TO         SHO-F04.
     MOVE       NK3-F15       TO         SHO-F031.
     MOVE       NK3-F16       TO         SHO-F032.
     PERFORM    HSHOTBL-READ-SEC.
*
 NYKFILL3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
*I--------------------------MOVE  1?      TO  MEI-F07  入数
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
     IF       HMEIMS-INV-FLG NOT = SPACE
              DISPLAY NC"商品名称マスタ未登録！"     UPON CONS
              DISPLAY NC"商品ＣＤ＝" NK3-F15 NK3-F16 UPON CONS
***           MOVE  4000    TO  PROGRAM-STATUS
***           MOVE  "END"   TO  END-FLG
***           GO            TO  HMEIMS-READ-EXIT
     END-IF.
*
*    MOVE     " "   TO  TANE-SIZAI.
*    IF   HMEIMS-INV-FLG = SPACE
*         MOVE   "2"   TO  TANE-SIZAI
*         IF  MEI-F09 = "01" OR "02" OR "03" OR "04" OR
*                       "05" OR "06" OR "07" OR "08"
*             MOVE   "1"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
*         END-IF
*         IF  MEI-F09 = "13" OR "14"
*             MOVE   "2"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
*         END-IF
*    END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 HSHOTBL-READ-SEC        SECTION.

     READ  HSHOTBL
       INVALID
         MOVE  "INV"             TO  HSHOTBL-INV-FLG
         MOVE  "0"               TO  SHO-F10
       NOT INVALID
         MOVE  SPACE             TO  HSHOTBL-INV-FLG
     END-READ.
 HSHOTBL-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
