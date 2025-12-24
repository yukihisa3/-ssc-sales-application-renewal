# SBT0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0030B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ抽出（ジュンテンドー*
*    作成日／更新日　　　：　2012/10/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを売上伝票データファイル*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2013/02/18                        *
*　　更新日／更新者　　　：　INOUE                             *
*    変更概要　　　　　　：　張替区分２の条件追加　　　　　　  *
*      　　　　　　　　　　　部門コード・形状コード転送変更　　*
*　　更新日／更新者　　　：　2022/03/14                        *
*　　更新日／更新者　　　：　NAV TAKAHASHI                     *
*    変更概要　　　　　　：　２０分類変更い伴い改修　　　　　  *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0030B.
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
*売上伝票データ
     SELECT   JTCDENF   ASSIGN    TO        DA-01-VI-JTCDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-ST.
*出荷連携データ
     SELECT   LNKSYKF   ASSIGN    TO        DA-01-S-LNKSYKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYU-ST.
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
     SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JTCDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     JTCDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷連携データ
******************************************************************
 FD  LNKSYKF           BLOCK     CONTAINS   25  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     LNKSYKF  OF        XFDLIB
              JOINING   SYU       PREFIX.
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
 01  WK-SUURYOU              PIC  999999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  DEN-ST        PIC  X(02).
     03  SYU-ST        PIC  X(02).
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
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  SYU-ERR           PIC  N(20)  VALUE
         NC"出荷連携データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SHO-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0030B".
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
     USE         AFTER     EXCEPTION PROCEDURE JTCDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKSYKF.
     DISPLAY     SYU-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYU-ST    UPON      CONS.
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
     OPEN     I-O       JTCDENF.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     EXTEND    LNKSYKF.
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
     START    JTCDENF  KEY  >=    DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*売上伝票ファイル読込
     PERFORM  JTCDENF-READ-SEC.
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
*出荷区分
     MOVE     "01"                TO   SYU-F01.
*納品日
     MOVE     DEN-F112            TO   WK-DEN-F112.
     MOVE     WK-DEN-F112         TO   SYU-F02.
*伝票番号　　
     MOVE     DEN-F02             TO   SYU-F03.
*取引先コード
     MOVE     DEN-F01             TO   SYU-F04.
*店舗コード
     MOVE     DEN-F70             TO   SYU-F05.
*受信日
     MOVE     DEN-F46             TO   SYU-F06.
*受信時刻
     MOVE     DEN-F47             TO   SYU-F07(1:4).
     MOVE     "00"                TO   SYU-F07(5:2).
*届先コード
     MOVE     SPACE               TO   SYU-F08.
*仕分店コード１
     MOVE     DEN-F70             TO   SYU-F09.
*仕分店コード２
     MOVE     DEN-F07             TO   SYU-F10.
*部門コード
*    2013/02/19↓変更
*****MOVE     SPACE               TO   SYU-F11.
     MOVE     "000"               TO   SYU-F11(1:3).
     MOVE     DEN-F71             TO   SYU-F11(4:1).
     MOVE     SPACE               TO   SYU-F11(5:1).
*    2013/02/19↑変更
*
*形状コード　
*    2013/02/19↓変更
*****MOVE     SPACE               TO   SYU-F12.
     MOVE     DEN-F72             TO   SYU-F12.
*    2013/02/19↑変更
*
*明細行番号
     MOVE     "0"                 TO   SYU-F13(1:1).
     MOVE     DEN-F03             TO   SYU-F13(2:2).
*品名コード
*2013/02/18↓条件追加
*****IF       MEI-F06     =       SPACE
*****         GO          TO      MAIN-99
*****END-IF.
*****MOVE     MEI-F06             TO   SYU-F14.
*
     IF       SHO-F10     =       "2"
              MOVE       DEN-F25  TO   SYU-F14
     ELSE
              IF         MEI-F06  =    SPACE
                     DISPLAY  NC"ＪＡＮＣＤ取得エラー！！"

                       " !"   NC"取＝" DEN-F01
                        "!"   NC"伝＝" DEN-F02
                        "!"   NC"行＝" DEN-F03
                        "!"   NC"店＝" DEN-F07
                        "!"   NC"納＝" DEN-F112
                        "!"   NC"サ商＝" DEN-F141
                        "!"   NC"相商＝" DEN-F25
                         GO       TO   MAIN-99
              ELSE
                         MOVE     MEI-F06   TO   SYU-F14
              END-IF
     END-IF.
*2013/02/18↑条件追加
*出荷指示数
     IF       MEI-F89   =   "1"
              DIVIDE  DEN-F50  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI   NOT = 0
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     MOVE    DEN-F50    TO  WK-SUURYOU
                     DISPLAY NC"数量が入数で割り切れません！"

                       " !"  NC"商Ｃ＝" MEI-F01
                        "!"  NC"入数＝" WK-MEI-F07
                        "!"  NC"数量＝" WK-SUURYOU
                        "!"  NC"取Ｃ＝" DEN-F01
                        "!"  NC"店舗＝" DEN-F07
                        "!"  NC"納日＝" DEN-F112
                        "!"  NC"伝票＝" DEN-F02
                        "!"  NC"行＝" DEN-F03
***                  MOVE     4000       TO  PROGRAM-STATUS
***                  MOVE     "END"      TO  END-FLG
***                  GO       TO             MAIN-EXIT
                     GO       TO             MAIN-99
              END-IF
     ELSE
              MOVE    DEN-F50             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F15.
*実出荷数
     IF       MEI-F89   =   "1"
              DIVIDE  DEN-F15  BY MEI-F07  GIVING  WK-DEN-F50
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     MOVE    DEN-F15    TO  WK-SUURYOU
                     DISPLAY  NC"数量が入数で割り切れません！"

                        " !"  NC"商Ｃ＝" MEI-F01
                         "!"  NC"入数＝" WK-MEI-F07
                         "!"  NC"数量＝" WK-SUURYOU
                         "!"  NC"取Ｃ＝" DEN-F01
                         "!"  NC"店舗＝" DEN-F07
                         "!"  NC"納日＝" DEN-F112
                         "!"  NC"伝票＝" DEN-F02
                         "!"  NC"行＝" DEN-F03
***                   MOVE     4000       TO  PROGRAM-STATUS
***                   MOVE     "END"      TO  END-FLG
***                   GO       TO             MAIN-EXIT
                      GO       TO             MAIN-99
              END-IF
     ELSE
              MOVE    DEN-F15             TO  WK-DEN-F50
     END-IF.
     MOVE     WK-DEN-F50          TO   SYU-F16.
*ラベル張り替え区分
*2013/02/18↓条件追加
*****IF       SHO-F10     =       SPACE
     IF       SHO-F10     =       " "      OR   "2"
*2013/02/18↑条件追加
              MOVE     "0"                 TO   SYU-F17
     ELSE
              MOVE     SHO-F10             TO   SYU-F17
     END-IF.
*張り替えＪＡＮコード
     IF       SHO-F10     =       "1"
              MOVE     SHO-F02             TO   SYU-F18
*2013/02/18↓追加
     ELSE
              MOVE     SPACE               TO   SYU-F18
*2013/02/18↑追加
     END-IF.
*種資材区分
     MOVE     TANE-SIZAI          TO   SYU-F19.
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
     PERFORM  JTCDENF-READ-SEC.
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
         MOVE "B"                       TO PARA-OUT-SYURUI
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
     CLOSE     JTCDENF   LNKSYKF  HMEIMS   HSHOTBL.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　売上伝票ファイル読込
****************************************************************
 JTCDENF-READ-SEC          SECTION.
*
     READ     JTCDENF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO JTCDENF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*指定バッチ_チェック
 JTCDENF-READ-1.
     IF     ( PARA-IN-ONL-JUSIN-HI  =  DEN-F46 ) AND
            ( PARA-IN-ONL-JUSIN-JI  =  DEN-F47 ) AND
            ( PARA-IN-ONL-JUSIN-TOR =  DEN-F01 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   JTCDENF-READ-EXIT
     END-IF.
*抽出倉庫チェック（オンライン：振分倉庫）
 JTCDENF-READ-2.
     IF       PARA-IN-SOUKO  =   DEN-F48
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   JTCDENF-READ-EXIT
     END-IF.
*納品日範囲チェック
 JTCDENF-READ-3.
     IF       PARA-IN-ONL-NOUHIN-FR   >    DEN-F112
              GO                  TO   JTCDENF-READ-SEC
     END-IF.
     IF       PARA-IN-ONL-NOUHIN-TO   <    DEN-F112
              GO                  TO   JTCDENF-READ-SEC
     END-IF.
*店舗範囲チェック
 JTCDENF-READ-4.
     IF       PARA-IN-ONL-TENPO-FR    >    DEN-F07
              GO                  TO   JTCDENF-READ-SEC
     END-IF.
     IF       PARA-IN-ONL-TENPO-TO    <    DEN-F07
              GO                  TO   JTCDENF-READ-SEC
     END-IF.
*物流連携ＦＬＧチェック
 JTCDENF-READ-5.
*  新規：未連携が対象
*  再送：連携済が対象
     IF       PARA-IN-SOUSIN-KB       =   " "
        IF       DEN-F68                 =   "1"
                 GO                  TO   JTCDENF-READ-SEC
        END-IF
     ELSE
        IF       DEN-F68             NOT =   "1"
                 GO                  TO   JTCDENF-READ-SEC
        END-IF
     END-IF.
*商品変換ＴＢＬ取得
 JTCDENF-READ-8.
     PERFORM    HSHOTBL-READ-SEC.
     IF         HSHOTBL-INV-FLG   NOT =  SPACE
                GO            TO         JTCDENF-READ-SEC
     END-IF.
*商品名称マスタ取得
 JTCDENF-READ-7.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         JTCDENF-READ-SEC
     END-IF.
*
 JTCDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     MOVE     DEN-F1411     TO         MEI-F011.
     MOVE     DEN-F1412     TO         MEI-F012.
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
*I--------------------------MOVE  ?????   TO  TANE-SIZAI 種資材
*I--------------------------MOVE  ?????   TO  MEI-F89  束区分
*I--------------------------MOVE  1       TO  MEI-F07  入数
                            DISPLAY  NC"商品名称マスタ未登録！"

                                     NC"　商Ｃ＝" DEN-F14

***                         MOVE  4000    TO  PROGRAM-STATUS
***                         MOVE  "END"   TO  END-FLG
                            GO            TO  HMEIMS-READ-EXIT
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
     MOVE     " "   TO  TANE-SIZAI.
     IF   HMEIMS-INV-FLG = SPACE
          MOVE   "2"   TO  TANE-SIZAI
*#2022/03/14 NAV ST
**********IF  MEI-F09 = "01" OR "02" OR "03" OR "04" OR
*                       "05" OR "06" OR "07" OR "08"
*             MOVE   "1"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
*         END-IF
*         IF  MEI-F09 = "13" OR "14"
*             MOVE   "2"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
**********END-IF
          IF  MEI-F09 = "01" OR "02" OR "03"
              MOVE   "1"   TO  TANE-SIZAI
              GO     TO    HMEIMS-READ-EXIT
          END-IF
          IF  MEI-F09 = "05"
              MOVE   "2"   TO  TANE-SIZAI
              GO     TO    HMEIMS-READ-EXIT
          END-IF
*#2022/03/14 NAV ED
     END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE  DEN-F01               TO  SHO-F01.
     MOVE  DEN-F25               TO  SHO-F02.
     READ  HSHOTBL
       INVALID
         DISPLAY  NC"商品変換ＴＢＬ未登録！"
             " !" NC"取Ｃ＝" DEN-F01
              "!" NC"商Ｃ＝" DEN-F25
         MOVE  "INV"             TO  HSHOTBL-INV-FLG
***      MOVE  4000              TO  PROGRAM-STATUS
***      MOVE  "END"             TO  END-FLG
         GO                      TO  HSHOTBL-READ-EXIT
*I-------MOVE  "0"               TO  SHO-F10 ラベル張替区分
       NOT INVALID
         MOVE  SPACE             TO  HSHOTBL-INV-FLG
     END-READ.
     IF  DEN-F1411 = SPACE
         MOVE  SHO-F031          TO  DEN-F1411
         MOVE  SHO-F032          TO  DEN-F1412
     END-IF.
 HSHOTBL-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
