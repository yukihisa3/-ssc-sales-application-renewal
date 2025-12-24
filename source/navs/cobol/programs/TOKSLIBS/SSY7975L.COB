****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　ダイキオンライン                  *
*    業務名　　　　　　　：　センター納品対応　　　　　　　　　*
*    モジュール名　　　　：　荷個数明細書出力　                *
*    作成日／更新日　　　：　06/08/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　                                  *
*    再利用ＰＧ　　　　　：                                    *
*2007/11/19 DCMJAPANへの変更による発注種別区分修正             *
*2008/02/20 部門CD変更
****************************************************************
****************************************************************
 IDENTIFICATION          DIVISION.
****************************************************************
 PROGRAM-ID.             SSY7975L.
 AUTHOR.                 NAV
 DATE-WRITTEN.           06.08.02.
****************************************************************
 ENVIRONMENT             DIVISION.
****************************************************************
 CONFIGURATION           SECTION.
 SOURCE-COMPUTER.        FACOM.
 OBJECT-COMPUTER.        FACOM.
 SPECIAL-NAMES.
         CONSOLE         IS             CONS.
*
 INPUT-OUTPUT            SECTION.
 FILE-CONTROL.
*    センター納品データ
     SELECT   DSCENTF      ASSIGN     TO    DA-01-VI-DSCENTL1
                           ORGANIZATION     INDEXED
                           ACCESS     MODE  SEQUENTIAL
                           RECORD     KEY   DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F16   DKC-F07
                           FILE    STATUS   DKC-STATUS.
*    店舗マスタ
     SELECT   HTENMS       ASSIGN     TO      DA-01-VI-TENMS1
                           ORGANIZATION       INDEXED
                           ACCESS     MODE    RANDOM
                           RECORD     KEY     TEN-F52  TEN-F011
                           FILE    STATUS     TEN-STATUS.
*    プリントファイル
     SELECT  PRTF      ASSIGN    TO             GS-PRTF
                       DESTINATION              "PRT"
                       FORMAT                   PRT-FORM
                       GROUP                    PRT-GRP
                       PROCESSING               PRT-PROC
                       UNIT CONTROL             PRT-CTL
                       FILE STATUS              PRT-STATUS.
*=============================================================*
 DATA                    DIVISION.
*=============================================================*
 FILE                    SECTION.
*    センター納品データ
 FD  DSCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DSCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
*    店舗マスタ
 FD  HTENMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*    帳票ファイル
 FD  PRTF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY79751  OF        XMDLIB
     JOINING     PRT       AS        PREFIX.
*
*=============================================================*
 WORKING-STORAGE          SECTION.
*=============================================================*
*    制御領域
 01  STATUS-AREA.
     03  DKC-STATUS               PIC  X(02).
     03  TEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
*    ＦＯＲＭ制御領域
 01  PRT-FORM                     PIC  X(08).
 01  PRT-PROC                     PIC  X(02).
 01  PRT-GRP                      PIC  X(08).
 01  PRT-CTL.
     03  PRT-CNTRL                PIC  X(04).
     03  PRT-STR-PG               PIC  X(02).
*    フラグエリア
 01  FLG-AREA.
     03  FLG-END                  PIC  X(03)  VALUE  SPACE.
     03  FLG-READ                 PIC  X(03)  VALUE  SPACE.
*    退避エリア
 01  SAV-AREA.
     03  WK-NOUDATE               PIC  9(08)  VALUE  ZERO.
     03  WK-TORIHIKI              PIC  9(08)  VALUE  ZERO.
     03  WK-TENPO                 PIC  9(05)  VALUE  ZERO.
     03  WK-HACKBN                PIC  X(01)  VALUE  SPACE.
     03  PAGE-CNT                 PIC  9(04)  VALUE  ZERO.
     03  TATE                     PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(02)  VALUE  ZERO.
     03  IY                       PIC  9(02)  VALUE  ZERO.
     03  IZ                       PIC  9(01)  VALUE  ZERO.
     03  WK-GK-SURYO              PIC  9(07)  VALUE  ZERO.
     03  WK-DATE                  PIC  9(01)  VALUE  ZERO.
     03  READ-CNT                 PIC  9(06)  VALUE  ZERO.
*    エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*    システム日付
 01  WRK-DATE.
     03  WRK-DATE1                PIC  9(02)  VALUE  ZERO.
     03  WRK-DATE2                PIC  9(06)  VALUE  ZERO.
*
*    メッセージ　エリア
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-PG-ID            PIC  X(08)  VALUE  "SSY7975L".
         05  FILLER               PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
         05  FILLER               PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID            PIC  X(08).
         05  FILLER               PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD             PIC  X(02).
         05  FILLER               PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                 SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
*============================================================*
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO.
*============================================================*
 DECLARATIVES.
*    プリントファイル
 FILEERR-SEC1           SECTION.
     USE AFTER EXCEPTION PROCEDURE   PRTF.
     MOVE      "PRTF"           TO   ERR-FL-ID.
     MOVE      PRT-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　センター納品データ
 FILEERR-SEC2           SECTION.
     USE AFTER EXCEPTION PROCEDURE   DSCENTF.
     MOVE      "DSCENTF"        TO   ERR-FL-ID.
     MOVE      DKC-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
*　　店舗マスタ
 FILEERR-SEC3           SECTION.
     USE AFTER EXCEPTION PROCEDURE   HTENMS.
     MOVE      "HTENMS"         TO   ERR-FL-ID.
     MOVE      TEN-STATUS       TO   ERR-STCD.
     DISPLAY   MSG-ABEND1       UPON CONS.
     DISPLAY   MSG-ABEND2       UPON CONS.
     DISPLAY   SEC-NAME         UPON CONS.
     MOVE     "4000"            TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
*
*============================================================*
*　　ゼネラル処理　　　　　　　　　　　　  構造■0.0         *
*============================================================*
 CONTROL-SEC             SECTION.
     MOVE     "COTROL-SEC"        TO   S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL    FLG-END  =  "END".
     PERFORM  END-SEC.
     STOP     RUN.
*
 CONTROL-EXIT.
     EXIT.
*============================================================*
*　　初期処理　　　　　　　　　　　　　　  構造■1.0         *
*============================================================*
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*    使用ファイル　ＯＰＥＮ
     OPEN     INPUT     DSCENTF  HTENMS.
     OPEN     OUTPUT    PRTF.
*    システム日付の取得
*システム日付・時刻の取得
     ACCEPT   WRK-DATE2         FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WRK-DATE2           TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WRK-DATE.
*    プリントエリア初期化
     MOVE     SPACE          TO   PRT-FSY79751.
     MOVE     ZERO           TO   READ-CNT.
*
*    センター納品データスタート
     MOVE     SPACE          TO   DKC-REC.
     INITIALIZE                   DKC-REC.
     MOVE     PARA-JDATE     TO   DKC-F01.
     MOVE     PARA-JTIME     TO   DKC-F02.
     MOVE     PARA-TORICD    TO   DKC-F03.
     MOVE     PARA-SOKO      TO   DKC-F04.
     START    DSCENTF   KEY  >=   DKC-F01   DKC-F02
                                  DKC-F03   DKC-F04
                                  DKC-F05   DKC-F06
                                  DKC-F16   DKC-F07
         INVALID   KEY
              MOVE    "END"  TO   FLG-END
              GO   TO   INIT-EXT
     END-START.
*    センター納品データ初期ＲＥＡＤ
     PERFORM  DSCENTF-RD-SEC.
     MOVE     DKC-F05    TO       WK-NOUDATE.
     MOVE     DKC-F06    TO       WK-TENPO.
     MOVE     DKC-F16    TO       WK-HACKBN.
*    取引先退避
     MOVE     DKC-F03    TO       WK-TORIHIKI.
***  MOVE     1          TO       TATE.
     MOVE     0          TO       TATE.
*
 INIT-EXT.
     EXIT.
*============================================================*
*　　センター納品データ読込み　　　　　　  構造■            *
*============================================================*
 DSCENTF-RD-SEC       SECTION.
     MOVE     "DSCENTF-RD-SEC"   TO   S-NAME.
*    センター納品データ読込み
     READ     DSCENTF  NEXT  AT  END
              IF READ-CNT > ZERO
                 MOVE      WK-GK-SURYO     TO     PRT-SOUGOK
                 PERFORM   SYUKEI-WT-SEC
              END-IF
              MOVE     "END"      TO       FLG-END
              GO                  TO       DSCENTF-RD-EXIT
     END-READ.
 READ010.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DKC-F01
     AND      PARA-JTIME  =  DKC-F02
     AND      PARA-TORICD =  DKC-F03
              CONTINUE
     ELSE
              MOVE     "END"     TO   FLG-END
              GO                 TO   DSCENTF-RD-EXIT
     END-IF.
     IF       PARA-SOKO   =  SPACE
              GO                 TO   DSCENTF-RD-010
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =    DKC-F04
              GO                  TO   DSCENTF-RD-010
     ELSE
              GO                  TO   DSCENTF-RD-SEC
     END-IF.
 DSCENTF-RD-010.
*    対象データ件数カウント
     ADD      1                   TO   READ-CNT.
*
 DSCENTF-RD-EXIT.
     EXIT.
*============================================================*
*　　メイン処理　　　　　　　　　　　　　  構造■2.0         *
*============================================================*
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*    ブレイクチェック（納品日，店舗）
     IF       WK-NOUDATE  NOT =  DKC-F05
     OR       WK-TENPO    NOT =  DKC-F06
     OR       WK-HACKBN   NOT =  DKC-F16
              MOVE  WK-GK-SURYO  TO  PRT-SOUGOK
              PERFORM            SYUKEI-WT-SEC
              MOVE  DKC-F05      TO  WK-NOUDATE
              MOVE  DKC-F06      TO  WK-TENPO
              MOVE  DKC-F16      TO  WK-HACKBN
**            MOVE  1            TO  TATE
              MOVE  0            TO  TATE
              MOVE  ZERO         TO  WK-GK-SURYO
     END-IF.
*    店舗、納品日
     IF       WK-NOUDATE     =   DKC-F05
         AND  WK-TENPO       =   DKC-F06
              ADD   1            TO  TATE
*    同頁内で部門が２０アイテム以上の時，次頁へ改頁
              IF    TATE  >  20
                    PERFORM          SYUKEI-WT-SEC
                    MOVE  DKC-F05    TO  WK-NOUDATE
                    MOVE  DKC-F06    TO  WK-TENPO
                    MOVE  DKC-F16    TO  WK-HACKBN
**                  MOVE  1          TO  TATE
                    MOVE  0          TO  TATE
                    MOVE  ZERO       TO  WK-GK-SURYO
              END-IF
     END-IF.
*    項目セット処理へ
     PERFORM  DATA-SET-SEC.
*    センター納品データ読込み
     PERFORM  DSCENTF-RD-SEC.
*
 MAIN-EXT.
     EXIT.
*============================================================*
*　　終了処理　　　　　　　　　　　　　　  構造■3.0         *
*============================================================*
 END-SEC                 SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*    使用ファイルＣＬＯＳＥ
     CLOSE               DSCENTF  HTENMS  PRTF.
*    終了メッセージ
     DISPLAY "************************" UPON  CONS.
     DISPLAY "*    荷個数明細書      *" UPON  CONS.
     DISPLAY "*  ｼｭﾂﾘｮｸ ｹﾝｽｳ = " PAGE-CNT "  *"  UPON  CONS.
     DISPLAY "************************" UPON  CONS.
*
 END-EXT.
     EXIT.
*============================================================*
*    荷個数明細書出力処理                  構造■2.1         *
*============================================================*
 SYUKEI-WT-SEC         SECTION.
     MOVE     "SYUKEI-WT-SEC"     TO   S-NAME.
*    ページ　カウントアップ
     ADD      1        TO        PAGE-CNT.
*****MOVE   PAGE-CNT   TO        PRT-HDPAGE.
*    システム日付セット
*****MOVE   WRK-DATE    TO       PRT-HDDATE.
*ヘッダ
     MOVE   WK-TORIHIKI           TO   TEN-F52.
     MOVE   WK-TENPO              TO   TEN-F011.
     READ   HTENMS
         INVALID   KEY
            MOVE   SPACE    TO  PRT-CANTNM
         NOT  INVALID  KEY
            MOVE   TEN-F02  TO  PRT-CANTNM
     END-READ.
*****MOVE    NC"定番"                 TO   PRT-KBN.
*
     EVALUATE WK-HACKBN
         WHEN "1"   MOVE NC"定番"           TO   PRT-KBN
         WHEN "2"   MOVE NC"特売"           TO   PRT-KBN
         WHEN "3"   MOVE NC"本部発注"       TO   PRT-KBN
         WHEN "4"   MOVE NC"新店発注"       TO   PRT-KBN
         WHEN "5"   MOVE NC"増床発注"       TO   PRT-KBN
*2007/11/19 DCMJAPANへの変更による発注種別区分追加対応
         WHEN "6"   MOVE NC"特注客注"       TO   PRT-KBN
         WHEN OTHER MOVE ALL NC"＊"         TO   PRT-KBN
     END-EVALUATE.
*    納品日
     MOVE    DKC-F05(1:4)             TO   PRT-NOUYY.
     MOVE    DKC-F05(5:2)             TO   PRT-NOUMM.
     MOVE    DKC-F05(7:2)             TO   PRT-NOUDD.
*    仕入先ＣＤ
     MOVE    100403                   TO   PRT-TOKUCD.
*    仕入先名
     MOVE    NC"（株）サカタのタネ"   TO   PRT-TOKUNM.
*
*    印字制御項目セット
     MOVE     SPACE               TO   PRT-PROC.
     MOVE     SPACE               TO   PRT-CTL.
     MOVE     SPACE               TO   PRT-FORM.
     MOVE    "FSY79751"           TO   PRT-FORM.
     MOVE    "SCREEN"             TO   PRT-GRP.
*    荷個数明細書出力
 SYUKEI010.
     WRITE    PRT-FSY79751.
*    プリントエリア初期化
     MOVE     SPACE               TO   PRT-FSY79751.
*
 SYUKEI-WT-EXIT.
     EXIT.
*============================================================*
*    項目セット処理                        構造■2.2         *
*============================================================*
 DATA-SET-SEC            SECTION.
     MOVE     "DATA-SET-SEC"      TO   S-NAME.
*
     DISPLAY "TATE = " TATE UPON CONS.
*    明細
*    部門
     MOVE     DKC-F07        TO   PRT-BUMON(TATE)
**** EVALUATE     DKC-F07
******** WHEN    001
************* MOVE     NC"日曜大工"   TO   PRT-BUMONM(TATE)
******** WHEN    002
************* MOVE     NC"建築建物"   TO   PRT-BUMONM(TATE)
******** WHEN    003
************* MOVE     NC"木材・建材" TO   PRT-BUMONM(TATE)
******** WHEN    004
************* MOVE     NC"インテリア" TO   PRT-BUMONM(TATE)
******** WHEN    005
************* MOVE     NC"塗料接着剤" TO   PRT-BUMONM(TATE)
******** WHEN    006
************* MOVE     NC"家庭用品"   TO   PRT-BUMONM(TATE)
******** WHEN    007
************* MOVE     NC"住設配管"   TO   PRT-BUMONM(TATE)
******** WHEN    008
************* MOVE     NC"園芸用品"   TO   PRT-BUMONM(TATE)
******** WHEN    009
************* MOVE     NC"ペット用品" TO   PRT-BUMONM(TATE)
******** WHEN    010
************* MOVE     NC"電気用品"   TO   PRT-BUMONM(TATE)
******** WHEN    011
************* MOVE     NC"保安自転車" TO   PRT-BUMONM(TATE)
******** WHEN    012
************* MOVE     NC"エクステリア" TO PRT-BUMONM(TATE)
******** WHEN    013
************* MOVE     NC"カー用品"   TO   PRT-BUMONM(TATE)
******** WHEN    014
************* MOVE     NC"文教用品"   TO   PRT-BUMONM(TATE)
******** WHEN    015
************* MOVE     NC"花木"       TO   PRT-BUMONM(TATE)
******** WHEN    016
************* MOVE     NC"日用雑貨"   TO   PRT-BUMONM(TATE)
******** WHEN    018
************* MOVE     NC"収納用品"   TO   PRT-BUMONM(TATE)
******** WHEN    019
************* MOVE     NC"ガーデン用品" TO PRT-BUMONM(TATE)
******** WHEN    021
************* MOVE     NC"ライフ用品" TO   PRT-BUMONM(TATE)
******** WHEN    OTHER
************* MOVE     ALL NC"＊"     TO   PRT-BUMONM(TATE)
**** END-EVALUATE.
*
**08/02/20 STR
     EVALUATE     DKC-F07
         WHEN    001
              MOVE     NC"木材・建築資材"  TO   PRT-BUMONM(TATE)
         WHEN    002
              MOVE     NC"園芸"            TO   PRT-BUMONM(TATE)
         WHEN    003
              MOVE     NC"工具"            TO   PRT-BUMONM(TATE)
         WHEN    004
              MOVE     NC"金物"            TO   PRT-BUMONM(TATE)
         WHEN    005
              MOVE     NC"インテリア"      TO   PRT-BUMONM(TATE)
         WHEN    006
              MOVE     NC"カー用品"        TO   PRT-BUMONM(TATE)
         WHEN    007
              MOVE     NC"電材"            TO   PRT-BUMONM(TATE)
         WHEN    008
              MOVE     NC"日用消耗"        TO   PRT-BUMONM(TATE)
         WHEN    009
              MOVE     NC"スポーツ・玩具"  TO   PRT-BUMONM(TATE)
         WHEN    010
              MOVE     NC"文具"            TO   PRT-BUMONM(TATE)
         WHEN    011
              MOVE     NC"ガーデンＥＸ"    TO   PRT-BUMONM(TATE)
         WHEN    012
              MOVE     NC"ペット"          TO   PRT-BUMONM(TATE)
         WHEN    013
              MOVE     NC"植物"            TO   PRT-BUMONM(TATE)
         WHEN    014
              MOVE     NC"家具・収納"      TO   PRT-BUMONM(TATE)
         WHEN    015
              MOVE     NC"塗料・補修"      TO   PRT-BUMONM(TATE)
         WHEN    016
              MOVE     NC"自転車・レジャー"
                                           TO   PRT-BUMONM(TATE)
         WHEN    017
              MOVE     NC"家電"            TO   PRT-BUMONM(TATE)
         WHEN    018
              MOVE     NC"ダイニングキッチン"
                                           TO   PRT-BUMONM(TATE)
         WHEN    019
              MOVE     NC"ソフトグッズ"    TO   PRT-BUMONM(TATE)
         WHEN    020
              MOVE     NC"カウンター用品・サングラス・喫煙具"
                                           TO   PRT-BUMONM(TATE)
         WHEN    022
              MOVE     NC"ＨＢＣ（ヘアケア・ボディ・車椅子）"
                                           TO   PRT-BUMONM(TATE)
         WHEN    023
              MOVE     NC"作業用品"        TO   PRT-BUMONM(TATE)
         WHEN    024
              MOVE     NC"リフォーム"      TO   PRT-BUMONM(TATE)
         WHEN    025
              MOVE     NC"寝具"            TO   PRT-BUMONM(TATE)
         WHEN    027
              MOVE     NC"照明"            TO   PRT-BUMONM(TATE)
         WHEN    028
              MOVE     NC"バス・トイレタリー"
                                           TO   PRT-BUMONM(TATE)
         WHEN    029
              MOVE     NC"ペットコンセ"    TO   PRT-BUMONM(TATE)
         WHEN    030
              MOVE     NC"食品"            TO   PRT-BUMONM(TATE)
         WHEN    031
              MOVE     NC"灯油"            TO   PRT-BUMONM(TATE)
         WHEN    034
              MOVE     NC"催事"            TO   PRT-BUMONM(TATE)
         WHEN    OTHER
              MOVE     NC"＊＊＊＊＊＊"    TO   PRT-BUMONM(TATE)
     END-EVALUATE.
*08/02/20 END
*
*    小物
     MOVE     DKC-F08                 TO   PRT-KOMONO(TATE).
*    異形
     MOVE     DKC-F09                 TO   PRT-IKEI(TATE).
*    ケース
     MOVE     DKC-F10                 TO   PRT-KESU(TATE).
*    他
     MOVE     DKC-F11                 TO   PRT-HOKA(TATE).
*    荷個数計
     COMPUTE  PRT-NIGOK(TATE) =  DKC-F08 + DKC-F09 +
                                 DKC-F10 + DKC-F11.
*
     ADD      PRT-NIGOK(TATE)    TO   WK-GK-SURYO.
*
 DATA-SET-EXIT.
     EXIT.
