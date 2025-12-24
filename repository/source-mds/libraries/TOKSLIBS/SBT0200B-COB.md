# SBT0200B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0200B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　入荷連携データ抽出（横持：振替）　*
*    作成日／更新日　　　：　2012/10/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを振替累積ファイル　　　*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2022/03/14 NAV TAKAHASHI          *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　２０分類変更に伴う改修            *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0200B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替累積ファイル２（送信区分＝新規　用）
     SELECT   FURRUIL2  ASSIGN    TO        DA-01-VI-FURRUIL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       FR2-F96   FR2-F02J
                                            FR2-F02I
                        FILE  STATUS   IS   FR2-ST.
*振替累積ファイル３（送信区分＝再送　用）
     SELECT   FURRUIL3  ASSIGN    TO        DA-01-VI-FURRUIL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       FR3-F96   FR3-F02J
                                            FR3-F02I  FR3-F97
                        FILE  STATUS   IS   FR3-ST.
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
     SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL8
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F031
                                            SHO-F0321
                                            SHO-F0322
                                            SHO-F0323
                        FILE      STATUS    IS   SHO-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替累積ファイル２
******************************************************************
 FD  FURRUIL2
                        LABEL RECORD   IS   STANDARD.
     COPY     FURRUIL2   OF        XFDLIB
              JOINING   FR2  AS   PREFIX.
*
******************************************************************
*    振替累積ファイル３
******************************************************************
 FD  FURRUIL3
                        LABEL RECORD   IS   STANDARD.
     COPY     FURRUIL3   OF        XFDLIB
              JOINING   FR3  AS   PREFIX.
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
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  FR2-ST        PIC  X(02).
     03  FR3-ST        PIC  X(02).
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
     03  FR2-ERR           PIC  N(20)  VALUE
         NC"振替累積ファイル２エラー".
     03  FR3-ERR           PIC  N(20)  VALUE
         NC"振替累積ファイル３エラー".
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
         05  ST-PG          PIC   X(08)  VALUE "SBT0200B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0200B".
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
 FR2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FURRUIL2.
     DISPLAY     FR2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FR2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FR3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FURRUIL3.
     DISPLAY     FR3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FR3-ST    UPON      CONS.
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
*送信区分＝" "（新規）→Ｌ２　"1"再送→Ｌ３
     IF       PARA-IN-SOUSIN-KB   =    "1"
              OPEN      I-O       FURRUIL3
     ELSE
              OPEN      I-O       FURRUIL2
     END-IF.
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
*送信区分＝" "（新規）→Ｌ２　"1"再送→Ｌ３
     IF       PARA-IN-SOUSIN-KB  =  "1"
              GO                    TO   INIT-02
     ELSE
              GO                    TO   INIT-01
     END-IF.
*
 INIT-01.
*振替累積ファイル２スタート（送信区分＝" "（新規））
     MOVE     SPACE                 TO   FR2-REC.
     INITIALIZE                          FR2-REC.
     MOVE     SPACE                 TO   FR2-F96.
     MOVE     PARA-IN-SOUKO         TO   FR2-F02J.
     MOVE     1                     TO   FR2-F02I.
     START    FURRUIL2  KEY  >=   FR2-F96   FR2-F02J  FR2-F02I
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*振替累積ファイル２読込
     PERFORM  FURRUIL2-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
     GO       TO   INIT-EXIT.
*
 INIT-02.
*振替累積ファイル３スタート（送信区分＝"1"（再送））
     MOVE     SPACE                 TO   FR3-REC.
     INITIALIZE                          FR3-REC.
     MOVE     "1"                   TO   FR3-F96.
     MOVE     PARA-IN-SOUKO         TO   FR3-F02J.
     MOVE     1                     TO   FR3-F02I.
     MOVE     PARA-IN-YOK-HI        TO   FR3-F97.
     START    FURRUIL3  KEY  >=   FR3-F96   FR3-F02J
                                  FR3-F02I  FR3-F97
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*振替累積ファイル３読込
     PERFORM  FURRUIL3-READ-SEC.
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
*送信区分＝" "（新規）→Ｌ２　"1"再送→Ｌ３
     IF       PARA-IN-SOUSIN-KB  =  "1"
              PERFORM   MAIN-L3-SEC
     ELSE
              PERFORM   MAIN-L2-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（振替累積ファイルＬ２より抽出）　　　*
****************************************************************
 MAIN-L2-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*入荷区分
*↓とりあえず動作確認用
*I   MOVE     "03" ???            TO   NYU-F01.
     MOVE     "02"                TO   NYU-F01.
*入荷日
*↓とりあえず動作確認用
*I   MOVE     FR2-F028???         TO   NYU-F02.
     MOVE     FR2-F028            TO   NYU-F02.
*伝票番号　　
*↓とりあえず動作確認用
*I   MOVE     FR2-F023???         TO   NYU-F03.
     MOVE     FR2-F023            TO   NYU-F03.
*入荷元コード
*↓とりあえず動作確認用
*I   MOVE     FR2-F026???         TO   NYU-F04.
     MOVE     FR2-F026            TO   NYU-F04.
*品名コード
*↓とりあえず動作確認用
*I   MOVE     FR2-F02A???         TO   NYU-F05.
*I            ↑もしくはSHO-F02 ?
     MOVE     SHO-F02             TO   NYU-F05.
*数量 ?入り数割りする？？？？
     IF       MEI-F89   =   "1"
              COMPUTE   FR2-F02F  =    FR2-F02F /  MEI-F07
     ELSE
              COMPUTE   FR2-F02F  =    FR2-F02F
     END-IF.
     MOVE     FR2-F02F            TO   WK-DEN-F15.
     MOVE     WK-DEN-F15          TO   NYU-F06.
*入荷連携データ出力
     WRITE    NYU-REC.
*振替累積ファイルＬ２へ連携済FLG更新
     MOVE     "1"                 TO   FR2-F96.
     MOVE     SYS-DATEW           TO   FR2-F97.
     REWRITE  FR2-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*振替累積ファイルＬ２読込
     PERFORM  FURRUIL2-READ-SEC.
*
 MAIN-L2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（振替累積ファイルＬ３より抽出）　　　*
****************************************************************
 MAIN-L3-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   NYU-REC.
     INITIALIZE                        NYU-REC.
*入荷区分
*↓とりあえず動作確認用
*I   MOVE     "03" ???            TO   NYU-F01.
     MOVE     "02"                TO   NYU-F01.
*入荷日
*↓とりあえず動作確認用
*I   MOVE     FR3-F028???         TO   NYU-F02.
     MOVE     FR3-F028            TO   NYU-F02.
*伝票番号　　
*↓とりあえず動作確認用
*I   MOVE     FR3-F023???         TO   NYU-F03.
     MOVE     FR3-F023            TO   NYU-F03.
*入荷元コード
*↓とりあえず動作確認用
*I   MOVE     FR3-F026???         TO   NYU-F04.
     MOVE     FR3-F026            TO   NYU-F04.
*品名コード
*↓とりあえず動作確認用
*I   MOVE     FR3-F02A???         TO   NYU-F05.
*I            ↑もしくはSHO-F02 ?
     MOVE     SHO-F02             TO   NYU-F05.
*
*数量 ?入り数割りする？？？？
     IF       MEI-F89   =   "1"
              COMPUTE   FR3-F02F  =    FR3-F02F /  MEI-F07
     ELSE
              COMPUTE   FR3-F02F  =    FR3-F02F
     END-IF.
     MOVE     FR3-F02F            TO   WK-DEN-F15.
     MOVE     WK-DEN-F15          TO   NYU-F06.
*入荷連携データ出力
     WRITE    NYU-REC.
*振替累積ファイルＬ３へ連携済FLG更新
     MOVE     "1"                 TO   FR3-F96.
     MOVE     SYS-DATEW           TO   FR3-F97.
     REWRITE  FR3-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*振替累積ファイルＬ３読込
     PERFORM  FURRUIL3-READ-SEC.
*
 MAIN-L3-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*ＰＡＲＡ（ＯＵＴ）セット
     IF  WRT-CNT1 > ZERO
         MOVE "J"                       TO PARA-OUT-SYURUI
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
               CLOSE     FURRUIL3
     ELSE
               CLOSE     FURRUIL2
     END-IF.
     CLOSE     LNKNYKF  HMEIMS   HSHOTBL.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　振替累積ファイル２読込（送信区分＝" "新規 用）
****************************************************************
 FURRUIL2-READ-SEC          SECTION.
*
     READ     FURRUIL2
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO FURRUIL2-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（" "ブレイクで終了）
     IF       FR2-F96    NOT  =  " "
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL2-READ-EXIT
     END-IF.
*場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       FR2-F02J   NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL2-READ-EXIT
     END-IF.
*入庫/出庫/発注区分チェック（1ブレイクで終了）
     IF       FR2-F02I   NOT  =   1
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL2-READ-EXIT
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  FR2-F11  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  FURRUIL2-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       FR2-F02A      TO         MEI-F011.
     MOVE       FR2-F02B      TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
*商品変換ＴＢＬ取得
     MOVE       FR2-F02A      TO         SHO-F031.
     MOVE       FR2-F02B      TO         SHO-F032.
     PERFORM    HSHOTBL-READ-SEC.
*
 FURRUIL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　振替累積ファイル３読込（送信区分＝"1"再送 用）
****************************************************************
 FURRUIL3-READ-SEC          SECTION.
*
     READ     FURRUIL3
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO FURRUIL3-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（"1"ブレイクで終了）
     IF       FR3-F96    NOT  =  "1"
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL3-READ-EXIT
     END-IF.
*場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       FR3-F02J   NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL3-READ-EXIT
     END-IF.
*入庫/出庫/発注区分チェック（1ブレイクで終了）
     IF       FR3-F02I   NOT  =   1
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL3-READ-EXIT
     END-IF.
*物流連携日（ＰＡＲＡ横持日ブレイクで終了）
     IF       FR3-F97    NOT  =   PARA-IN-YOK-HI
              MOVE      "END"     TO   END-FLG
              GO                  TO   FURRUIL3-READ-EXIT
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  FR3-F11  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  FURRUIL3-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       FR3-F02A      TO         MEI-F011.
     MOVE       FR3-F02B      TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
*商品変換ＴＢＬ取得
     MOVE       FR3-F02A      TO         SHO-F031.
     MOVE       FR3-F02B      TO         SHO-F032.
     PERFORM    HSHOTBL-READ-SEC.
*
 FURRUIL3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
*↓とりあえず動作確認用
*I                          MOVE  ?????   TO  TANE-SIZAI 種資材
*I                          MOVE  ?????   TO  MEI-F89  束区分
*I                          MOVE  1       TO  MEI-F07  入数
                            MOVE  "1"     TO  TANE-SIZAI
                            MOVE  " "     TO  MEI-F89
                            MOVE  1       TO  MEI-F07
*↑とりあえず動作確認用
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
     MOVE     " "   TO  TANE-SIZAI.
     IF       HMEIMS-INV-FLG = SPACE
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
     READ  HSHOTBL
       INVALID
         MOVE  "INV"             TO  HSHOTBL-INV-FLG
*↓とりあえず動作確認用
*I       MOVE  "0"               TO  SHO-F10 ラベル張替区分
*I       MOVE  "???????????????" TO  SHO-F02 ｾｯﾄするはずの品名CD
         MOVE  "0"               TO  SHO-F10
         MOVE  "????????????"    TO  SHO-F02
*↑とりあえず動作確認用
       NOT INVALID
         MOVE  SPACE             TO  HSHOTBL-INV-FLG
*↓とりあえず動作確認用
*I       MOVE  張替ＪＡＮコード？
*↑とりあえず動作確認用
     END-READ.
 HSHOTBL-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
