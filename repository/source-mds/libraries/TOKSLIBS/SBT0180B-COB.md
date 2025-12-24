# SBT0180B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0180B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　入荷連携データ抽出（横持：入出庫）*
*    作成日／更新日　　　：　2012/10/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを入出庫ファイル　　　　*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　                                  *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0180B.
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
*入出庫ファイル４（送信区分＝新規　用）
     SELECT   NYSFILL4  ASSIGN    TO        DA-01-VI-NYSFILL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NY4-F94   NY4-F11
                                            NY4-F04
                        FILE  STATUS   IS   NY4-ST.
*入出庫ファイル６（送信区分＝再送　用）
     SELECT   NYSFILL6  ASSIGN    TO        DA-01-VI-NYSFILL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NY6-F94   NY6-F11
                                            NY6-F95
                        FILE  STATUS   IS   NY6-ST.
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
*    SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL8
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       SHO-F031
*                                           SHO-F0321
*                                           SHO-F0322
*                                           SHO-F0323
*                       FILE      STATUS    IS   SHO-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    入出庫ファイル４
******************************************************************
 FD  NYSFILL4
                        LABEL RECORD   IS   STANDARD.
     COPY     NYSFILL4   OF        XFDLIB
              JOINING   NY4  AS   PREFIX.
*
******************************************************************
*    入出庫ファイル６
******************************************************************
 FD  NYSFILL6
                        LABEL RECORD   IS   STANDARD.
     COPY     NYSFILL6   OF        XFDLIB
              JOINING   NY6  AS   PREFIX.
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
*FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
*    COPY     HSHOTBL    OF       XFDLIB
*             JOINING   SHO       PREFIX.
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
 01  WK-SUURYOU              PIC  9999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  NY4-ST        PIC  X(02).
     03  NY6-ST        PIC  X(02).
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
     03  NY4-ERR           PIC  N(20)  VALUE
         NC"入出庫ファイル４エラー".
     03  NY6-ERR           PIC  N(20)  VALUE
         NC"入出庫ファイル６エラー".
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
         05  ST-PG          PIC   X(08)  VALUE "SBT0180B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0180B".
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
 NY4-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NYSFILL4.
     DISPLAY     NY4-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NY4-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 NY6-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NYSFILL6.
     DISPLAY     NY6-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NY6-ST    UPON      CONS.
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
*SHO-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
*    DISPLAY     SHO-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     SHO-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
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
*送信区分＝" "（新規）→Ｌ４　"1"再送→Ｌ６
     IF       PARA-IN-SOUSIN-KB   =    "1"
              OPEN      I-O       NYSFILL6
     ELSE
              OPEN      I-O       NYSFILL4
     END-IF.
     OPEN     INPUT     HMEIMS.
*    OPEN     INPUT     HSHOTBL.
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
*送信区分＝" "（新規）→Ｌ４　"1"再送→Ｌ６
     IF       PARA-IN-SOUSIN-KB  =  "1"
              GO                    TO   INIT-02
     ELSE
              GO                    TO   INIT-01
     END-IF.
*
 INIT-01.
*入出庫ファイル４スタート（送信区分＝" "（新規））
     MOVE     SPACE                 TO   NY4-REC.
     INITIALIZE                          NY4-REC.
     MOVE     SPACE                 TO   NY4-F94.
     MOVE     PARA-IN-SOUKO         TO   NY4-F11.
     MOVE     "I3"                  TO   NY4-F04.
     START    NYSFILL4  KEY  >=   NY4-F94   NY4-F11   NY4-F04
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*入出庫ファイル４読込
     PERFORM  NYSFILL4-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
     GO       TO   INIT-EXIT.
*
 INIT-02.
*入出庫ファイル６スタート（送信区分＝"1"（再送））
     MOVE     SPACE                 TO   NY6-REC.
     INITIALIZE                          NY6-REC.
     MOVE     "1"                   TO   NY6-F94.
     MOVE     PARA-IN-SOUKO         TO   NY6-F11.
     MOVE     PARA-IN-YOK-HI        TO   NY6-F95.
     START    NYSFILL6  KEY  >=   NY6-F94   NY6-F11   NY6-F95
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*入出庫ファイル６読込
     PERFORM  NYSFILL6-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
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
*送信区分＝" "（新規）→Ｌ４　"1"再送→Ｌ６
     IF       PARA-IN-SOUSIN-KB  =  "1"
              PERFORM   MAIN-L6-SEC
     ELSE
              PERFORM   MAIN-L4-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（入出庫ファイルＬ４より抽出）　　　　*
****************************************************************
 MAIN-L4-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*入荷区分
     MOVE     "02"                TO   NYU-F01.
*入荷日
     MOVE     NY4-F15             TO   NYU-F02.
*伝票番号　　
     MOVE     NY4-F02             TO   NYU-F03.
*入荷元コード
     MOVE     NY4-F10             TO   NYU-F04.
*品名コード
     IF       MEI-F06   =         SPACE
              GO        TO        MAIN-L4-99
     END-IF.
     MOVE     MEI-F06             TO   NYU-F05.
*数量
*    IF       MEI-F89   =   "1"
*             COMPUTE   NY4-F12   =    NY4-F12  /  MEI-F07
*    ELSE
*             COMPUTE   NY4-F12   =    NY4-F12
*    END-IF.
*    MOVE     NY4-F12             TO   WK-DEN-F15.
*    MOVE     WK-DEN-F15          TO   NYU-F06.
     IF       MEI-F89   =   "1"
              DIVIDE  NY4-F12  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     DISPLAY  NC"数量が入数で割り切れません！"
                                                      UPON CONS
                     DISPLAY  NC"商品ＣＤ＝" MEI-F01  UPON CONS
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     DISPLAY NC"入り数　＝" WK-MEI-F07 UPON CONS
                     MOVE    NY4-F12    TO  WK-SUURYOU
                     DISPLAY NC"数量　　＝" WK-SUURYOU UPON CONS
                     DISPLAY  NC"作業日　＝" NY4-F15  UPON CONS
                     DISPLAY  NC"伝票　　＝" NY4-F02  UPON CONS
                     DISPLAY  NC"入荷元　＝" NY4-F10  UPON CONS
***                   MOVE     4000       TO  PROGRAM-STATUS
***                   MOVE     "END"      TO  END-FLG
***                   GO       TO             MAIN-EXIT
                      GO       TO             MAIN-L4-99
              END-IF
     ELSE
              MOVE    NY4-F12             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   NYU-F06.
*入荷連携データ出力
     WRITE    NYU-REC.
*入出庫ファイル４へ連携済FLG更新
     MOVE     "1"                 TO   NY4-F94.
     MOVE     SYS-DATEW           TO   NY4-F95.
     REWRITE  NY4-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*入出庫ファイル４読込
 MAIN-L4-99.
     PERFORM  NYSFILL4-READ-SEC.
*
 MAIN-L4-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（入出庫ファイルＬ６より抽出）　　　　*
****************************************************************
 MAIN-L6-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*入荷区分
     MOVE     "02"                TO   NYU-F01.
*入荷日
     MOVE     NY6-F15             TO   NYU-F02.
*伝票番号　　
     MOVE     NY6-F02             TO   NYU-F03.
*入荷元コード
     MOVE     NY6-F10             TO   NYU-F04.
*品名コード
     IF       MEI-F06   =         SPACE
              GO        TO        MAIN-L6-99
     END-IF.
     MOVE     MEI-F06             TO   NYU-F05.
*数量
*    IF       MEI-F89   =   "1"
*             COMPUTE   NY6-F12   =    NY6-F12  /  MEI-F07
*    ELSE
*             COMPUTE   NY6-F12   =    NY6-F12
*    END-IF.
*    MOVE     NY6-F12             TO   WK-DEN-F15.
*    MOVE     WK-DEN-F15          TO   NYU-F06.
     IF       MEI-F89   =   "1"
              DIVIDE  NY6-F12  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     DISPLAY  NC"数量が入数で割り切れません！"
                                                      UPON CONS
                     DISPLAY  NC"商品ＣＤ＝" MEI-F01  UPON CONS
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     DISPLAY NC"入り数　＝" WK-MEI-F07 UPON CONS
                     MOVE    NY6-F12    TO  WK-SUURYOU
                     DISPLAY NC"数量　　＝" WK-SUURYOU UPON CONS
                     DISPLAY  NC"作業日　＝" NY6-F15  UPON CONS
                     DISPLAY  NC"伝票　　＝" NY6-F02  UPON CONS
                     DISPLAY  NC"入荷元　＝" NY6-F10  UPON CONS
***                   MOVE     4000       TO  PROGRAM-STATUS
***                   MOVE     "END"      TO  END-FLG
***                   GO       TO             MAIN-EXIT
                      GO       TO             MAIN-L6-99
              END-IF
     ELSE
              MOVE    NY6-F12             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   NYU-F06.
*入荷連携データ出力
     WRITE    NYU-REC.
*入出庫ファイル６へ連携済FLG更新
     MOVE     "1"                 TO   NY6-F94.
     MOVE     SYS-DATEW           TO   NY6-F95.
     REWRITE  NY6-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*入出庫ファイル６読込
 MAIN-L6-99.
     PERFORM  NYSFILL6-READ-SEC.
*
 MAIN-L6-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*ＰＡＲＡ（ＯＵＴ）セット
     IF  WRT-CNT1 > ZERO
         MOVE "H"                       TO PARA-OUT-SYURUI
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
     IF        PARA-IN-SOUSIN-KB    =    "1"
               CLOSE     NYSFILL6
     ELSE
               CLOSE     NYSFILL4
     END-IF.
     CLOSE     LNKNYKF  HMEIMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　入出庫ファイル４読込（送信区分＝" "新規 用）
****************************************************************
 NYSFILL4-READ-SEC          SECTION.
*
     READ     NYSFILL4
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO NYSFILL4-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（" "ブレイクで終了）
     IF       NY4-F94    NOT  =  " "
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL4-READ-EXIT
     END-IF.
*入庫場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       NY4-F11    NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL4-READ-EXIT
     END-IF.
*作業区分（"I3"ブレイクで終了）
     IF       NY4-F04    NOT  =   "I3"
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL4-READ-EXIT
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  NY4-F12  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  NYSFILL4-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       NY4-F05       TO         MEI-F011.
     MOVE       NY4-F06       TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         NYSFILL4-READ-SEC
     END-IF.
*商品変換ＴＢＬ取得
*    MOVE       NY4-F05       TO         SHO-F031.
*    MOVE       NY4-F06       TO         SHO-F032.
*    PERFORM    HSHOTBL-READ-SEC.
*
 NYSFILL4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　入出庫ファイル６読込（送信区分＝"1"再送 用）
****************************************************************
 NYSFILL6-READ-SEC          SECTION.
*
     READ     NYSFILL6
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO NYSFILL6-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（"1"ブレイクで終了）
     IF       NY6-F94    NOT  =  "1"
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL6-READ-EXIT
     END-IF.
*入庫場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       NY6-F11    NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL6-READ-EXIT
     END-IF.
*物流連携日（ＰＡＲＡ横持日ブレイクで終了）
     IF       NY6-F95    NOT  =   PARA-IN-YOK-HI
              MOVE      "END"     TO   END-FLG
              GO                  TO   NYSFILL6-READ-EXIT
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  NY6-F12  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  NYSFILL6-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       NY6-F05       TO         MEI-F011.
     MOVE       NY6-F06       TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         NYSFILL6-READ-SEC
     END-IF.
*商品変換ＴＢＬ取得
*    MOVE       NY6-F05       TO         SHO-F031.
*    MOVE       NY6-F06       TO         SHO-F032.
*    PERFORM    HSHOTBL-READ-SEC.
*
 NYSFILL6-READ-EXIT.
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
        IF    PARA-IN-SOUSIN-KB    =    "1"
              DISPLAY NC"商品名称マスタ未登録！"     UPON CONS
              DISPLAY NC"商品ＣＤ＝" NY6-F05 NY6-F06 UPON CONS
***           MOVE  4000    TO  PROGRAM-STATUS
***           MOVE  "END"   TO  END-FLG
              GO            TO  HMEIMS-READ-EXIT
        ELSE
              DISPLAY NC"商品名称マスタ未登録！"     UPON CONS
              DISPLAY NC"商品ＣＤ＝" NY4-F05 NY4-F06 UPON CONS
***           MOVE  4000    TO  PROGRAM-STATUS
***           MOVE  "END"   TO  END-FLG
              GO            TO  HMEIMS-READ-EXIT
        END-IF
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
*HSHOTBL-READ-SEC        SECTION.
*
*    READ  HSHOTBL
*      INVALID
*        MOVE  "INV"             TO  HSHOTBL-INV-FLG
*        MOVE  "0"               TO  SHO-F10
*      NOT INVALID
*        MOVE  SPACE             TO  HSHOTBL-INV-FLG
*    END-READ.
*HSHOTBL-READ-EXIT.
*    EXIT.
*-------------< PROGRAM END >------------------------------------*

```
